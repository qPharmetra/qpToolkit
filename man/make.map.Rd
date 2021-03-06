% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.map.r
\name{make.map}
\alias{make.map}
\title{Meta-analysis data tabulation}
\usage{
make.map(ds, cNm = "drug.abbr", trNm = "ref.tr")
}
\arguments{
\item{ds}{meta-analysis dataset}

\item{cNm}{column names of comparators}

\item{trNm}{column names of reference drugs}
}
\value{
A dataset summarizing the dimensions of the meta-analysis dataset

Placeholder for return description
}
\description{
Compose table of all two-way combinations of comparators and references
}
\note{
not intended to be used as standalone. Is called from qP function meta.map()
}
\examples{
test = c("Ref", paste("comp", 1:5, sep = ""))
test.ds = data.frame(drg = rep(test, times = seq(12,2,-2)),
                     ref.id = c(rep(1:4, 10),1,1))

names(test.ds)
meta.map(test.ds, compNm = "drg", trialNm = "ref.id", refNm = "Ref", dist = 1.4)
}
\seealso{
\code{\link{meta.map}}
}
