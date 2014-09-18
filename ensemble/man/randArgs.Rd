\name{randArgs}
\alias{randArgs}
\title{
Helper function for optParams
}
\description{
Gnerates random initial parameters, typically for use in optParams.
}
\usage{
randArgs(optArgs)
}
\arguments{
  \item{optArgs}{
A list containing the arguments to be optimized.  See optParams.
}
}
\value{
A random set of parameters for use in optParams.
}
\author{
Josh Browning (rockclimber112358@gmail.com)
}
\note{
Typically, this function will not be called directly by the user but rather via optParams.
}