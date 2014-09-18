\name{makeEnsem}
\alias{makeEnsem}
\title{
Combine multiple predictions into a final ensemble prediction.
}
\description{
This function accepts multiple models, fit on the training data and estimated on the testing data (typically via a function such as cvModel).  It generates an ensemble prediction by combining these models in an intelligent way.  Note that this function reads in all files in the Submissions directory of the form '_raw.csv', which should be the predictions from the ensemble models.  These files should contain all the predictions to be ensembled together.
}
\usage{
makeEnsem(actual, lossFunc = function(preds, actual) {
    mean((preds - actual)^2)
}, numIters = 100, topN = 10, prune = 0.7, baseLoss = 0.5)
}
\arguments{
  \item{actual}{
A numeric vector with the observed dependent variable values (for both the test and training data).  This vector should have observations in the same order as all the prediction files.
}
  \item{lossFunc}{
A loss function to measure the performance of the fit.  Smaller values indicate better performance.
}
  \item{numIters}{
What are the maximum number of iterations this algorithm should iterate through?  At each iteration, an additional model may be averaged to the current predictions.
}
  \item{topN}{
The initial predictions are formed using the topN best models (i.e. models with the lowest lossFunc value).  The default value of 10 is a good choice, at least if you have many models to ensemble together.
}
  \item{prune}{
How many models should be removed from consideration?  The worst prune \% of models are removed.
}
  \item{baseLoss}{
What is the loss of a naive prediction (i.e. the mean for all observations)?  This is used to throw out really bad models.
}
}
\details{
This function generates an ensemble prediction by first taking the best topN models and combining them.  Then, a new ensemble is considered by performing a weighted average of w times the current forecast and 1 times a new model (where w=topN).  If any of these averages beat the current ensemble (in terms of loss evaluated on the cross-validated training sets), that ensemble is stored as the best.  Either way, w is increased by 1.  The algorithm proceeds until numIters is reached.
}
\value{
\item{preds}{The prediction of the ensemble model on all observations (cross-validated as well as testing observations).}
\item{weights}{Models selected at each step to build the ensemble.}
}
\author{
Josh Browning (rockclimber112358@gmail.com)
}

\seealso{
cvModel
}
\examples{
d = data.frame( x1=rnorm(1000), x2=rnorm(1000), y=rnorm(1000) )
#Assume last 100 rows are test data
cvGroup = c(sample(1:10,size=900,replace=T), rep(-1,100))
d$y[901:1000] = NA
out = cvModel( modelFunc=glm
    ,cvGroup=cvGroup
    ,form="y ~ x1 + x2"
    ,d=d )
library(glmnet)
out2 = cvModel( modelFunc=glmnet
    ,cvGroup=cvGroup
    ,x=d[,1:2]
    ,y=d$y
    ,predFunc=function(fit, newdata){predict(fit, newx=newdata, s=c(4,2,1,.5))}
    ,pred.cols=4)
dir.create("Submissions")
write.csv(out$ensemble, file="Submissions/glm_raw.csv", row.names=F)
write.csv(out2$ensemble, file="Submissions/glmnet_raw.csv", row.names=F)
ensem = makeEnsem(actual=d$y, prune=.2)
}
\keyword{ensemble}
\keyword{cross-validate}
