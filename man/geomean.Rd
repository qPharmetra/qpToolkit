% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geomean.r
\name{geomean}
\alias{geomean}
\title{Geometric Mean}
\usage{
geomean(x, na.rm = FALSE)
}
\arguments{
\item{x}{numeric value}

\item{na.rm}{logical (default to TRUE) to omit NA entries}
}
\value{
Geometric mean of vector x
}
\description{
Geometric Mean
}
\examples{
set.seed(1234)
x = rlnorm(1000)
mean(x)
geomean(x)
}
