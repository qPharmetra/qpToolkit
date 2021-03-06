% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nls.vcov.r
\name{nls.vcov}
\alias{nls.vcov}
\title{NLS variance covariance matrix}
\usage{
nls.vcov(object)
}
\arguments{
\item{object}{an nls object}
}
\value{
Placeholder for return description
}
\description{
Extracts the variance covariance matrix from an nls object
}
\examples{
  DNase1 <- subset(DNase, Run == 1)
fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
summary(fm1DNase1)
nls.vcov(fm1DNase1)
}
