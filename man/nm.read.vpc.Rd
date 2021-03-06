% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm.read.vpc.r
\name{nm.read.vpc}
\alias{nm.read.vpc}
\title{Read VPC output}
\usage{
nm.read.vpc(
  path = getOption("nmDir"),
  vpc_results = "vpc_results.csv",
  vpctab = dir(path = path, pattern = "^vpctab")[1],
  PI.limits = c(0.025, 0.05, 0.1, 0.3, 0.7, 0.9, 0.95, 0.975),
  PI.ci.area.smooth = FALSE
)
}
\arguments{
\item{path}{directory where vpc_results.csv and vpctab files reside}

\item{vpc_results}{file name of summarized VPC results. Defaults to vpc_results.csv}

\item{vpctab}{filename of observed data table. Defaults to vpctab}

\item{PI.limits}{what prediction interval stats should be read from the VPC output files}

\item{PI.ci.area.smooth}{if TRUE the center of the interval of the independent variable will be used. This will create smooth VPC plots, the same way as xpose does. (defaults to FALSE)}
}
\value{
A list of data frames 'obs', 'vpc, and 'res'
}
\description{
Parses the output from PsN's vpc and prepares it for plotting.
}
\note{
Please note that qPharmetra's default VPC plot tool is xpose.VPC. Use this function with the \code{ggvpc} functions in case one needs more flexibility to post-process results before plotting, and when stratifcation by more than one variable is needed.
}
\examples{
myVPC = nm.read.vpc(path = file.path(getOption("qpExampleDir"), "vpc_final_strt"))

unique(myVPC$vpc$strata)
unique(myVPC$obs$strata)
# they match nicely
}
\seealso{
\code{\link{ggvpc_xpose}},  \code{\link{ggvpc_standard}}, \code{\link{read.vpc}}
}
