% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.shrinkage.r
\name{get.shrinkage}
\alias{get.shrinkage}
\title{NONMEM shrinkage estimates}
\usage{
get.shrinkage(run, path = getOption("nmDir"), file.ext = ".lst")
}
\arguments{
\item{run}{run rootname (e.g. \code{run1})}

\item{path}{directory where \code{run} resides}

\item{file.ext}{extension of the NONMEM output file (.ext)}
}
\value{
shrinkage values
}
\description{
Parse shrinkage values from NONMEM output file
}
\examples{
get.shrinkage(run = "example1", path = getOption("qpExampleDir"))
}
\seealso{
\code{\link{get.nm.version}}, \code{\link{get.ofv}}, \code{\link{read.out}}
}
