% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.demoTable.r
\name{format.demoTable}
\alias{format.demoTable}
\title{Format a tabulated summary for LaTeX}
\usage{
\method{format}{demoTable}(x, ..., formula)
}
\arguments{
\item{x}{output from \code{\link{tabSummarize}} of class \code{demoTable}}

\item{\dots}{ignored}

\item{formula}{the same formula as used to create the \code{tabSummarize} output}
}
\value{
Table with summarized data formatted for inclusion in LaTeX (character matrix)
}
\description{
Format a tabulated summary for LaTeX
}
\note{
This function is primarily used for demographics tables
}
\examples{
options(width = 150)
pkpdData = example.pkpdData()
ok = duplicated(pkpdData$id) == FALSE
myFormula =  dose ~ race + wt + bmi + sex
my.formatted.demoTable = format(
 tabSummarize(formula = myFormula, data = pkpdData[ok, ], digits = 3),
  formula = myFormula
)
library(xtable)
my.xtable <- xtable(my.formatted.demoTable)
print(my.xtable
  , sanitize.text.function = identity
  , booktabs = TRUE
)
}
\seealso{
\code{\link{tabStats}}, \code{\link{tabSummarize}}
}
