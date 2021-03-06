% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulate.samples.r
\name{tabulate.samples}
\alias{tabulate.samples}
\title{Summarize endpoints for numeric/non-numeric}
\usage{
tabulate.samples(data, analyte = "conc", oddCode = c("BLOQ", "NS", "M"))
}
\arguments{
\item{data}{dataset with endpoints to summarize}

\item{analyte}{the endpoint to analyze, e.g. "conc" or "effect"}

\item{oddCode}{charatcer vector with entries that are not considered numeric like 'BLOQ", "<LOQ", or "Missing Sample"}
}
\value{
Named vector of numeric and oddCode entries
}
\description{
Given a dataframe and items that describe it, create a tabular summary of any column that contains samples of a analyte from a drug trial, including a summary of any special values.
}
\examples{
my.df = data.frame( analyte.1 = c(1,2,3,4,'BLOQ',5,6,'NS',7,'M'),
                   analyte.2 = c(1,2,'BLOQ','M',3,'BLOQ',5,6,'NS',7))
tabulate.samples(data = my.df, analyte = "analyte.1")
tabulate.samples(data = my.df, analyte = "analyte.2")
}
\seealso{
\code{\link{whichNumeric}}
}
