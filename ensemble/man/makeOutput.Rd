\name{makeOutput}
\alias{makeOutput}
\title{
Generate output in Kaggle format
}
\description{
This function provides an easy interface for taking predictions and formatting the output for Kaggle submissions.  May need to be tweaked for different competitions.
}
\usage{
makeOutput(preds, call)
}
\arguments{
  \item{preds}{
Predicted values from a model or ensemble.
}
  \item{call}{
What was the call generating the model (for documentation purposes)?
}
}
\details{
Creates a new file in the submissions directory and assigns it an id.  Additionally, it adds an entry to the desc.csv file describing what model was fit.
}
\value{
Nothing is returned, but files are created/updated.  See details.
}
\author{
Josh Browning (rockclimber112358@gmail.com)
}