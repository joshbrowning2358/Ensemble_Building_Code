#func: function name (randomForest, gbm, rbf, etc.)
#form: Formula for the model fit.  Must be used with data and not x, y
#data: data used to specify the model.  Must be used with form and not x, y.
#x: independent variables for model.  Must be used with y and not data,form
#y: dependent variable for model.  Must be used with x and not data,form
#replications: how many random samples should be pulled for each test to verify model performance
#nTrain: vector of sample sizes to train on.  Should be something like 1000, 10000, ...  Idea is to optimize quickly on small samples and improve estimates on larger datasets.  
#optFunc: function used to optimize parameters.  Should have pred and actual as the only arguments.  This function will also have access to samVal, the sampled validation values.  So, you may use a weight from d, for example, via d$Weight[samVal], even if d$Weight isn't passed to this function.  Must create this so that minimum scores are best, can change max-best functions by adding a negative
#optVals: For numeric and ordered parameters, we optimize over seq(min, max,length.out=?) where min and max are specified.  This argument specifies the "?" for each case.  Ignored for "character" arguments
#predFunc: function used to predict onto training set.  Should take one argument: newdata
#constArgs: Should be a named list for arguments that should be passed to func
#coldStart: how many random cases to try before fixing the initial guess parameters
optParams = function(func, form=NULL, data=NULL, x=NULL, y=NULL
  ,nTrain=c(100,1000,10000), nValid=nTrain, replications=rep(30, length(nTrain))
  ,optFunc=function(pred,actual){mean((pred-actual)^2)}
  ,optArgs=list()
  ,optVals=rep(5,length(optArgs))
  ,optRed=rep(.7,length(optArgs))
  ,predFunc=predict
  ,constArgs=list()
  ,coldStart=10)
{
  #data quality checks
  if(!is(func,"function"))
    stop("func must be a function!")
  if(is.null(form) & (!is.null(data) | is.null(x) | is.null(y)))
    stop("Must specify form, data OR x, y")
  if(is.null(x) & (!is.null(y) | is.null(form) | is.null(data)))
    stop("Must specify form, data OR x, y")
  if(!is.null(x) & !is.null(form))
    stop("Must specify form, data OR x, y")
  if(!is.null(x))
    if(nrow(x)!=length(y))
      stop("Number of rows of x must be the same as length of y!")
  if(length(nTrain)!=length(nValid))
    stop("nTrain and nValid must have the same length!")
  n = ifelse(is.null(data), length(y), nrow(data))
  if(max(nTrain + nValid)>n)
    stop("nTrain + nValid exceeds n at some point!")
  if(length(optArgs)==0)
    stop("No arguments to optimize, length(optArgs)=0!")
  if(!is(optArgs,"list") | !all( lapply(optArgs, length)==3 ))
    stop("optArgs's arguments don't have the correct form!  Each should be a list of length 3.")
  if(!any( lapply(optArgs, function(x){
    (x[[2]] %in% c("numeric", "ordered") & length(x[[3]]==2)) |
    (x[[2]]=="categorical" & length(x[[3]])>1)}) ) )
    stop("optArgs is not of the right form!")
  #Testing functions to validate appropriate arguments

  library(ggplot2)
  
  bestError = Inf
  for(i in 1:coldStart ){
    tempArgs = randArgs(optArgs)
    samTrn = sample(n, size=nTrain[1])
    #Sample validation observations from 1:n, removing training obs
    samVal = sample((1:n)[-samTrn], size=nValid[1])
    
    #Start args with the baseArgs that you always need
    args = constArgs
    
    #Add the best estimates so far, except for the current parameter being optimized
    args = c(args, tempArgs)

    if(!is.null(form)){
      #Add the sampled data onto your args
      args = c(list(data=data[samTrn,]), args)
      args = c(list(form=form), args)

      fit = do.call(func, args)
      preds = predFunc(fit, newdata=data[samVal,])
      error = optFunc(preds, data[samVal,all.vars(form)[1]])
    } else {
      #Add the sampled x,y onto your args, make sure x and y come first
      args = c(list(y=y[samTrn]), args)
      args = c(list(x=x[samTrn,]), args)

      fit = do.call(func, args)
      preds = predFunc(fit, newdata=x[samVal,])
      error = optFunc(pred=preds, actual=y[samVal])
    }
    if(error<bestError){
      currArgs = tempArgs
      bestError = error
    }
  }
  
  #Main training loop
  for(epoch in 1:length(nTrain)){
    for(par in 1:length(optArgs)){
      
      currParam = optArgs[[par]][[1]]
      
      #Set the parameter values based on the desired types from optArgs
      paramVals = optArgs[[par]][[3]]
      if(optArgs[[par]][[2]]=="ordered"){
        paramVals = round(seq(paramVals[1], paramVals[2], length.out=optVals[par]))
        paramVals = unique(paramVals)
      }
      if(optArgs[[par]][[2]]=="numeric")
        paramVals = seq(paramVals[1], paramVals[2], length.out=optVals[par])
      
      errors = matrix(0, nrow=replications, ncol=length(paramVals))
      for( repl in 1:replications[epoch] ){
        samTrn = sample(n, size=nTrain[epoch])
        #Sample validation observations from 1:n, removing training obs
        samVal = sample((1:n)[-samTrn], size=nValid[epoch])
        for(parVal in 1:length(paramVals)){
          
          #Start args with the constArgs that you always need
          args = constArgs
          
          #Add the best estimates so far, except for the current parameter being optimized
          args = c(args, currArgs[names(currArgs)!=currParam] )

          #Add the parameter we're optimizing over
          args[[length(args)+1]] = paramVals[parVal]
          names(args)[length(args)] = currParam

          if(!is.null(form)){
            #Add the formula and sampled data onto your args
            args = c(list(data=data[samTrn,]), args)
            args = c(list(form=form), args)

            fit = do.call(func, args)
            preds = predFunc(fit, newdata=data[samVal,])
            errors[repl,parVal] = optFunc(preds, data[samVal,all.vars(form)[1]])
          } else {
            #Add the sampled x,y onto your args
            args = c(list(y=y[samTrn]), args)
            args = c(list(x=x[samTrn,]), args)

            fit = do.call(func, args)
            preds = predFunc(fit, newdata=x[samVal,])
            errors[repl,parVal] = optFunc(preds, y[samVal])
          }
        } #close parameter value loop
      } #close replication loop
      #Update currArgs with the best fit
      errors = apply(errors, 2, function(x){
        data.frame(mean=mean(x), sd=sd(x))})
      errors = do.call("rbind", errors)
      newPar = paramVals[which.min(errors[,1])]
      currArgs[currParam] = newPar
      #Update optArgs: tune parameter search as appropriate
      if(optArgs[[par]][[2]] %in% c("ordered", "numeric") ){
        oldRange = optArgs[[par]][[3]]
        len = oldRange[2] - oldRange[1]
        len = len*optRed[par]
        newRange = newPar + c(-len/2,len/2)
        
        #Move newRange if outside of original range
        if(newRange[1]<oldRange[1])
          newRange = newRange + oldRange[1] - newRange[1]
        if(newRange[2]>oldRange[2])
          newRange = newRange + oldRange[2] - newRange[2]
      } else {
        #categorical variables, remove ones from search that did significantly worse.
        best = errors[which.min(errors[,1]),]
        #Assume normality, then we're testing difference in means with different variances
        zVals = sapply( 1:nrow(errors), function(i){
          (errors[i,"mean"]-best["mean"])/sqrt(errors[i,"sd"]^2+best["sd"]^2) } )
        #Remove vals with z-scores greater than 2, i.e. alpha=.05 test
        optArgs[[par]][[3]] = optArgs[[par]][[3]][zVals<2]
      }
      
      ggsave(paste0("Optimization_Param_",currParam,"_epoch_",epoch,".png")
        ,qplot( x=paramVals, y=errors[,1] ) + labs(x=currParam) )
      print(paste("Parameter",currParam,"optimized to",paramVals[which.min(errors[,1])],"for epoch",epoch))
    } #close parameter loop
  } #close epoch loop
}

randArgs = function(optArgs){
  out = list()
  for(i in 1:length(optArgs)){
    vals = optArgs[[i]][[3]]
    if(optArgs[[i]][[2]]=="numeric")
      val = runif(1, min=vals[1], max=vals[2])
    if(optArgs[[i]][[2]]=="ordered")
      val = round(runif(1, min=vals[1]-.5, max=vals[2]+.5))
    if(optArgs[[i]][[2]]=="character")
      val = sample(vals, size=1)
    out[[length(out)+1]] = val
    names(out)[length(out)] = optArgs[[i]][[1]]
  }
  return(out)
}
