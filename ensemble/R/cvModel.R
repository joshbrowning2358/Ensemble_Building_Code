cvModel <-
function(modelFunc, cvGroup, predFunc=predict, d=NULL, form=NULL, x=NULL, y=NULL
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
    if(NROW(x)!=length(y))
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
    x = as.matrix(x)
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
      predData = x[cvGroup %in% c(-1,i),,drop=F]
    } else {
      newArgs = c(list(data=d[!cvGroup %in% c(-1,i),]), args)
      newArgs = c(list(form=form), newArgs)
      predData = d[cvGroup %in% c(-1,i),,drop=F]
    }
    rownames(predData) = rownames(ensem)[cvGroup %in% c(-1,i)]
    
    #Evaluate the model
    fit = do.call( modelFunc, args=newArgs )
    preds = predFunc(fit, newdata=predData )
    if(is.null(dim(preds)) | (is(preds,"array") & length(dim(preds))==1) )
      preds = matrix(preds,ncol=1)
    if(saveMods)
      mods[[length(mods)+1]] = fit
    rownames(preds) = rownames(predData)
    
    #Insert the predicted values for the cv group into the ensem data.frame.
    pred.index = (1:nrow(ensem))[cvGroup==i]
    ensem[pred.index,] = preds[rownames(preds) %in% pred.index,]
    
    #Insert the predicted values for the test group into the ensem data.frame, but divide by the number of cv-folds (since each fold has a diff. prediction)
    test.index = (1:nrow(ensem))[cvGroup==-1]
    if(length(test.index>0))
      ensem[test.index,] = ensem[test.index,] + preds[rownames(preds) %in% test.index,]/(length(unique(cvGroup))-1)
    print(paste0("Model ",i,"/",length(unique(cvGroup[cvGroup>0]))," has finished"))
  }
  return(list(ensemble=ensem, models=mods, call=sysCall))
}
