makeEnsem <-
function( actual, lossFunc=function(preds, actual){
    mean( (preds-actual)^2 )
  }
  , numIters=100, topN=10, prune=.7, baseLoss=.5){
  #Data quality check
  if(numIters<1)
    stop("numIters must be positive!")
  if(topN<0)
    stop("top must be non-negative!")
  if(prune>1 | prune<0)
    stop("prune must be between 0 and 1!")
  if(!"Submissions" %in% list.files())
    stop("No 'Submissions' directory!")
  
  trainRows = !is.na(actual)
  testRows = is.na(actual)
  
  setwd("Submissions")
  files = list.files()
  files = files[grepl("_raw", files)]
  if(length(files)==0){
    setwd("..")
    stop("No model files to combine from 'Submissions' directory!")
  }
  ensemDf = data.frame( read.csv(files[1]) )
  if(length(actual)!=nrow(ensemDf)){
    setwd("..")
    stop("length(actual)!=nrow(ensemDf).  Ensure actual has NA's for test obs!")
  }
  colnames(ensemDf) = gsub("_raw.csv", "", files[1])
  for(file in files[-1] ){
    ensemDf = cbind(ensemDf, read.csv( file ) )
    colnames(ensemDf)[ncol(ensemDf)] = gsub("_raw.csv", "", file)
  }
    
  lossVec = sapply( 1:ncol( ensemDf ), function(i){
    lossFunc( ensemDf[trainRows,i] , actual[trainRows] )
  } )
  #To avoid overfitting, prune the bottom end of the models (and anything with a worse than guessing metric).
  ensemDf = ensemDf[,lossVec>=baseLoss,drop=F]
  if(ncol(ensemDf)<2){
    setwd("..")
    stop("1 or fewer models had losses greater than baseLoss!  Ensembling is not possible.")
  }
  lossVec	= lossVec[lossVec>=baseLoss]
  ensemDf = ensemDf[,rank(lossVec)<=length(lossVec)*(1-prune),drop=F]
  if(ncol(ensemDf)<2){
    setwd("..")
    stop("1 or fewer models remain after pruning!  Ensembling is not possible.")
  }
  lossVec	= lossVec[rank(lossVec)<=length(lossVec)*(1-prune)]
  preds		= apply( ensemDf[,rank(lossVec)<=topN], 1, mean )	#Can let in more than top.N, depending on rank
  weights	= colnames(ensemDf)[rank(lossVec)<=topN]
  bestLoss= rep(lossFunc(preds[trainRows],actual[trainRows]), sum(rank(lossVec)<=topN))
  currLoss= lossFunc( preds[trainRows], actual[trainRows] )
  for( i in (length(weights)+1):numIters )
  {
    #Check if an improvement happened.  If so, append new values to vectors
    improvement = FALSE
    for( j in 1:ncol( ensemDf ) )
    {
      predsTemp	= preds*(i-1)/i + ensemDf[,j]/i
      lossTemp	= lossFunc( predsTemp[trainRows], actual[trainRows] )
      if( lossTemp < currLoss )
      {
        currLoss = lossTemp
        currPreds = predsTemp
        currVar = colnames(ensemDf)[j]
        improvement = TRUE
      }
    }
    preds		= currPreds
    weights = c(weights, ifelse(improvement, currVar, NA))
    bestLoss= c(bestLoss, currLoss)
  }

  #Move out of Submissions directory
  setwd("..")
  return( list(preds=preds, weights=weights, bestLoss=bestLoss) )
}
