% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.mod.r
\name{read.mod}
\alias{read.mod}
\title{Scan NONMEM output file}
\usage{
read.mod(run, path = getOption("nmDir"), file.ext = ".mod", quiet = TRUE)
}
\arguments{
\item{run}{run rootname (e.g. \code{run1})}

\item{path}{directory where \code{run} resides}

\item{file.ext}{file extension of the NONMEM output file (.mod)}

\item{quiet}{if TRUE no message will be returned.}
}
\value{
character vector with the NONMEM output. Each element i the vector represents a line in the NONMEM output file
}
\description{
Scan and parse the NONMEM control stream (mod file)
}
\examples{
head(read.mod(path = getOption("qpExampleDir"), run = "example1", file.ext = ".ctl"))
}
\seealso{
\code{\link{read.ext}}, \code{\link{read.out}}
}
