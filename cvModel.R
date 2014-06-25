#modelFunc: function for fitting the model.  Must take form, data or x,y as arguments, as well as args.
#cvGroup: vector of length=nrow(d) or nrow(x).  1-k specify the k cross-validation groups, -1 specifies the test data
#predFunc: function such that predFunc(fit, newdata) generates predictions for newdata.  newdata is of the same form as d or x.
#d: data.frame to use for model.  Must be specified with form and x,y must be NULL.
#form: model formula to use.  Must be specified with d and x,y must be NULL.
#x: independent variables for the model (i.e. a matrix/data.frame).  Must be specified with y and data,form must be NULL.
#y: dependent variable for the model (i.e. a vector).  Must be specified with x and data,form must be NULL.
#args: additional arguments to pass to modelFunc, in the form of a named list.
#pred.cols: How many prediction columns should be generated?  Usually this is 1, but in some special cases (i.e. gbm) multiple predictions are possible.  predFunc should be modified to return a matrix/data.frame with pred.cols number of columns.
#saveMods: set to true if a list of all the fitted models should be returned as well as the fitted, cross-validated predictions
#Change pred.cols to 10 for gbm and glmnet!!!
cvModel = function(modelFunc, cvGroup, predFunc=predict, d=NULL, form=NULL, x=NULL, y=NULL
        ,args=list(), pred.cols=1, saveMods=F, seed=321 ){
  sysCall = paste( deparse( sys.call() ), collapse=" ")
  
  #Data quality checks
  if(!is(modelFunc,"function"))
    stop("func must be a function!")
  if(is.null(form) & (!is.null(d) | is.null(x) | is.null(y)))
    stop("Must specify form, d OR x, y")
  if(is.null(x) & (!is.null(y) | is.null(form) | is.null(d)))
    stop("Must specify form, d OR x, y")
  if(!is.null(x) & !is.null(form))
    stop("Must specify form, d OR x, y")
  if(!is.null(x))
    if(nrow(x)!=length(y))
      stop("Number of rows of x must be the same as length of y!")
  n = ifelse( is.null(form), length(y), nrow(d) )
  if(length(cvGroup)!=n)
    stop("length(cvGroup)!=nrow(d).  Maybe cvGroup is a matrix?")  
  
  set.seed(seed)
  
  ensem = data.frame( matrix(0, nrow=n, ncol=pred.cols ) )
  colnames(ensem) = paste0("V",1:ncol(ensem))
  #Set up the rownames of ensem to match d. This makes inserting in predicted values much easier later:
  rownames(ensem) = 1:n
  if(is.null(form)){
    rownames(x) = 1:n
  } else {
    rownames(d) = 1:n
  }
  
  #Store the models, if desirable.  Create a mods list in either case, so return method is always consistent.
  mods = list()
  
  #Set up the model formula and rename the columns of d appropriately:
  #Holdout each cv-group in turn:
  for( i in sort(unique(cvGroup[cvGroup>0])) ){
    if(is.null(form)){
      newArgs = c(list(y=y[!cvGroup %in% c(-1,i)]), args)
      newArgs = c(list(x=x[!cvGroup %in% c(-1,i),]), newArgs)
      predData = x[cvGroup %in% c(-1,i),]
    } else {
      newArgs = c(list(data=d[!cvGroup %in% c(-1,i),]), args)
      newArgs = c(list(form=form), newArgs)
      predData = d[cvGroup %in% c(-1,i),]      
    }
    rownames(predData) = rownames(ensem)[cvGroup %in% c(-1,i)]
    
    #Evaluate the model
    fit = do.call( modelFunc, args=newArgs )
    preds = predFunc(fit, newdata=predData )
    if(is(preds,"numeric") | is(preds,"factor") | (is(preds,"array") & length(dim(preds))==1) )
      preds = matrix(preds,ncol=1)
    if(saveMods)
      mods[[length(mods)+1]] = fit
    rownames(preds) = rownames(predData)
    
    #Insert the predicted values for the cv group into the ensem data.frame.
    pred.index = (1:nrow(ensem))[cvGroup==i]
    ensem[pred.index,] = preds[rownames(preds) %in% pred.index,]
    
    #Insert the predicted values for the test group into the ensem data.frame, but divide by the number of cv-folds (since each fold has a diff. prediction)
    test.index = (1:nrow(ensem))[cvGroup==-1]
    ensem[test.index,] = ensem[test.index,] + preds[rownames(preds) %in% test.index,]/(length(unique(cvGroup))-1)
    print(paste0("Model ",i,"/",length(unique(cvGroup[cvGroup>0]))," has finished"))
  }
  return(list(ensemble=ensem, models=mods, call=sysCall))
}

#form: specify the model formula, i.e. Y ~ X1 + X2 + X3. Note that "." notation is supported.
#data: a dataframe containing the data for which the model is desired.
#hidden: the number of hidden neurons in the network. The package only supports one hidden layer.
#steps: how many iterations should be ran? Note this may need adjustment based on convergence.
#print: how many times should the function print the current fit number and LMS?
#...: other parameters to be passed into newff or train in the AMORE package.
fit.AMORE = function( form, data, hidden, steps=1000, print=10, learning.rate.global=1e-2, momentum.global=.5, report=T
                   ,error.criterium="LMS", Stao=NA, hidden.layer="tansig", output.layer="purelin", method="ADAPTgdwm" ){
  #Parse the form object to determine number of input neurons and relevant data
  if( !is(form, "formula") ) stop("Formula incorrectly specified")
  form = as.character( form )
  indCol = (1:ncol(data))[colnames(data)==form[2]]
  depVars = strsplit(form[3],"+", fixed=T)
  #period matches everything:
  if( depVars=="." ){
    depCols = (1:ncol(data))[-indCol]
  } else {
    depVars = sapply( depVars, function(x)gsub(" ","",x) )
    depCols = (1:ncol(data))[colnames(data) %in% depVars]
  }
  
  #Fit the neural network
  mod = newff( n.neurons=c(length(depCols)+1,hidden,1), learning.rate.global=1e-2, momentum.global=0.5,
               error.criterium="LMS", Stao=NA, hidden.layer="tansig",
               output.layer="purelin", method="ADAPTgdwm")
  mod.fit = train( mod, T=as.matrix(data[,indCol]), P=cbind(1,data[,depCols]), n.shows=print, show.step=steps/print, report=report )
  
  #Pull off the output weights
  weights = lapply( mod.fit$net$neurons, function(x)x$weights )
  hidden.wts = do.call( "rbind", weights[1:hidden] )
  output.wts = weights[[hidden+1]]
  return(list(hidden.wts=hidden.wts, output.wts=output.wts, activation.function=hidden.layer, form=form))
}

#mod: A list as output by fit.nn. It should contain hidden.wts, output.wts, activation.function and form. Custom activation functions are not supported!
#newdata: the data for which a prediction is desired.
predict.AMORE = function( mod, newdata ){
  newdata = cbind( 1, newdata )
  depVars = strsplit(mod$form[3],"+", fixed=T)
  depVars = sapply( depVars, function(x)gsub(" ","",x) )
  newdata = newdata[,colnames(newdata) %in% c("1",depVars)]
  if( mod$activation.function=="tansig" ){
    neurons = tanh( as.matrix(newdata) %*% t(mod$hidden.wts) )
    preds = neurons %*% mod$output.wts
  }
  if( mod$activation.function=="purelin" ){
    neurons = as.matrix(newdata) %*% t(mod$hidden.wts)
    preds = neurons %*% mod$output.wts
  }
  if( mod$activation.function=="sigmoid" ){
    neurons = 1/(1+exp(-as.matrix(newdata) %*% t(mod$hidden.wts)))
    preds = neurons %*% mod$output.wts
  }
  if( mod$activation.function=="hardlim" ){
    neurons = ifelse(as.matrix(newdata) %*% t(mod$hidden.wts)>0,1,0)
    preds = neurons %*% mod$output.wts
  }
  return(preds)
}
