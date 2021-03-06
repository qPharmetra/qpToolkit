% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logTicks.r
\name{logTicks}
\alias{logTicks}
\title{Define ticks for plots with log10 axes}
\usage{
logTicks(lim, loc = 1)
}
\arguments{
\item{lim}{the values to log base 10}

\item{loc}{a multiplier (see examples)}
}
\value{
Vector of log base 10 values to be used for axis tickmarks, not intended to be used as standalone function
}
\description{
Create a vector suitable for using as axis tickmarks in a graphic with a logged
}
\note{
This function is called by the scale functions for lo10 axes and is not to be used as standalone function.
}
\examples{
logTicks(lim = c(10,100), loc = 1)
logTicks(lim = c(100,10), loc = 1)
logTicks(lim = c(4,109,1000), loc = 1)
logTicks(lim = c(4,109,999), loc = 1)
logTicks(lim = c(109,109,1000,4), loc = 7)
logTicks(lim = c(109,109,1000,4), loc = 4)
logTicks(lim = c(109,109,1000,4), loc = 2)
logTicks(c(2.,5,10,23,532,140,1240,0.2,5,50), loc = c(1,3))
}
\seealso{
\code{\link{xscale.components.log10}} and \code{\link{yscale.components.log10}}
}
