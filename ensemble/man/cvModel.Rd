\name{cvModel}
\alias{cvModel}
\title{
Build an Cross-Validated Model
}
\description{
This function allows the user to specify a particular model (lm, randomForest, neuralnet, etc.) and easily build cross-validated models on the data.  Additionally, predictions from all 10 models are averaged on a test dataset for prediction or as an input to an ensemble model.
}
\usage{
cvModel(modelFunc, cvGroup, predFunc = predict, d = NULL, form = NULL, x = NULL, y = NULL, args = list(), pred.cols = 1, saveMods = F, seed = 321)
}
\arguments{
  \item{modelFunc}{
Function which specifies what model will be fit to the data.  Can be a built-in function or a custom, user-defined function; however, it must accept arguments d and form or x and y (depending on what is specified with this cvModel function) as well as any arguments passed via args).
}
  \item{cvGroup}{
A numeric vector of the same length as the number of rows of x and y or d.  It specifies which observations should belong to which cross-validation groups.  If data is supplied only for prediction purposes, than cvGroup should be -1 for those observations.
}
  \item{predFunc}{
A function which takes two arguments, fit and newdata, and returns a numeric vector or matrix of the same number of rows as newdata.  If multiple predictions are desirable, this function should return a matrix with multiple columns and pred.cols should be set accordingly.  Note that some functions default predict function will work here, but sometimes the user must write a wrapper to the predict function (usually adjusting argument names).
}
  \item{d}{
One of d and form OR x and y should be supplied.  d is the data.frame which contains the data for modeling.
}
  \item{form}{
One of d and form OR x and y should be supplied.  form is the formula which will be passed to modelFunc.
}
  \item{x}{
One of d and form OR x and y should be supplied.  x is the data.frame, matrix, or numeric vector of independent observations.
}
  \item{y}{
One of d and form OR x and y should be supplied.  y is the numeric vector of dependent observations to be modeled.
}
  \item{args}{
A list of additional (named) arguments to be supplied to modelFunc.
}
  \item{pred.cols}{
If predFunc returns a matrix with more than one column, then this should be set to the number of columns the user desires to save.
}
  \item{saveMods}{
Binary value indicating whether or not models should be saved.  The object returned from this function is a list, and one component is mods.  If saveMods=T, a list of all the models fit will be returned in mods.
}
  \item{seed}{
Random seed to set, if desirable.
}
}
\details{
This function uses the cross-validation groups supplied by the user to partition the data into several groups.  For each i in the cross-validation values, a model is built on all the data excluding i and -1 (the testing data).  This model is then used to predict on the testing data as well as the data with cv group=i.  Since multiple predictions will be made on the testing data, this function averages the predictions across all models.
}
\value{
A list with the following components:
\item{ensemble}{A matrix containing the predictions of the cross-validation models for all observations.}
\item{models}{If saveMods=T, then this element is a list of all the model objects fit to each of the cross-validation groups.  With large datasets, this object can be extremely large.}
\item{call}{The function call}.
}
\author{
Josh Browning (rockclimber112358@gmail.com)
}

\examples{
d = data.frame( x1=rnorm(1000), x2=rnorm(1000), y=rnorm(1000) )
out = cvModel( modelFunc=glm
    ,cvGroup=sample(1:10, size=1000, replace=TRUE)
    ,form="y ~ x1 + x2"
    ,d=d )
library(glmnet)
out = cvModel( modelFunc=glmnet
    ,cvGroup=sample(1:10, size=1000, replace=TRUE)
    ,x=d[,1:2]
    ,y=d$y
    ,predFunc=function(fit, newdata){predict(fit, newx=newdata, s=c(4,2,1,.5))}
    ,pred.cols=4)
}
\keyword{cross-validation}
\keyword{ensemble}
