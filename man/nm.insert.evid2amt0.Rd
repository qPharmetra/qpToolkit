% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm.insert.evid2amt0.r
\name{nm.insert.evid2amt0}
\alias{nm.insert.evid2amt0}
\title{Insert doses in other compartments}
\usage{
nm.insert.evid2amt0(data, ..., quiet = TRUE)
}
\arguments{
\item{data}{NONMEM data.frame, should contain variables}

\item{...}{(unquoted) columns to sort the result by}

\item{quiet}{when TRUE (default) silences the messages returned}
}
\value{
Data frame with an EVID = 2 and AMT = 0 record preceding each AMT>0 record
}
\description{
To model transit compartment absorption this function allows to turn a NONMEM datset into one EVID = 2 & AMT = 0 records before each dose given.
}
\examples{
library(dplyr)
nmData = example.NONMEM.dataset(TIME = seq(0,24,4))
nmData2 = nm.insert.evid2amt0(data = nmData, ID, TIME, -EVID)
tbl_df(nmData2[, c('ID','TIME','EVID','AMT','DV')])
}
\seealso{
\code{\link{nm.insert.dose.multiple.compartments}}
}
