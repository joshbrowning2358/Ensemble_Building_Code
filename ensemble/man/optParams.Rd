\name{optParams}
\alias{optParams}
\title{
Optimize model parameters
}
\description{
This function is designed to optimize the tuning parameters to a particular data mining model by building many models.  Note that it may be extremely slow, but should give good estimates for the optimal tuning parameters (by trying many combinations).
}
\usage{
optParams(func, form = NULL, data = NULL, x = NULL, y = NULL, nTrain = c(100, 1000, 10000), nValid = nTrain, replications = rep(30, length(nTrain)), optFunc = function(pred, actual) {
    mean((pred - actual)^2)
}, optArgs = list(), optVals = rep(5, length(optArgs)), optRed = rep(0.7, length(optArgs)), predFunc = predict, constArgs = list(), coldStart = 10, seed = 321)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func}{
The data mining function to be optimized.
}
  \item{form}{
Either form and d OR x and y should be supplied.  form supplies the formula to be used for fitting the model.
}
  \item{data}{
Either form and d OR x and y should be supplied.  d is a dataframe to be used for fitting the model.
}
  \item{x}{
Either form and d OR x and y should be supplied.  x is a matrix, dataframe, or numeric vector containing the independent variables for fitting.
}
  \item{y}{
Either form and d OR x and y should be supplied.  y is a numeric vector containing the dependent variable for fitting.
}
  \item{nTrain}{
The number of observations to be randomly sampled at each iteration (to build training models).
}
  \item{nValid}{
The number of observations to be randomly sampled at each iteration (to measure error of the trained models).
}
  \item{replications}{
Specifies how many iterations should be performed at each optimization step.  Typically 30 should be a good amount to ensure a good optimum is found, but decreasing this can help improve computation time.
}
  \item{optFunc}{
A function accepting two arguments: pred and actual.  From these two numeric vectors, the optFunc should provide a performance measure to be minimized.
}
  \item{optArgs}{
%%     ~~Describe \code{optArgs} here~~
}
  \item{optVals}{
%%     ~~Describe \code{optVals} here~~
}
  \item{optRed}{
%%     ~~Describe \code{optRed} here~~
}
  \item{predFunc}{
%%     ~~Describe \code{predFunc} here~~
}
  \item{constArgs}{
%%     ~~Describe \code{constArgs} here~~
}
  \item{coldStart}{
%%     ~~Describe \code{coldStart} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
