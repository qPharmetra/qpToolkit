% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.runrec.r
\name{process.runrec}
\alias{process.runrec}
\title{Format runrecord for reports}
\usage{
process.runrec(
  runrec,
  improvement = list(value = -6.63, color = "blue"),
  carryAlong = NULL,
  plain = FALSE
)
}
\arguments{
\item{runrec}{outut from \code{read.runrec}}

\item{improvement}{a list with elements value (numeric) and color (character) to emphasize significant improvements. Value defaults to -6.63, for a p<0.01 improvement for a single parameter added.}

\item{carryAlong}{any additional variable to be included (would need to be present in \code{runrec})}

\item{plain}{optional toggle to create a non-LaTeX formatted table}
}
\value{
Either a LaTeX preformatted runrecord table (default) or a fully processed runrecord table without LaTeX preformatting (for inclusion into Microsoft products).
}
\description{
Process the run record into a data frame ready for integration into LaTeX report. Note that the function assumes runrecord has been run with PsN option -maxlvl = 0 and the function depends on input from \code{read.runrec}
}
\examples{
rr = read.runrec(
 filename = "AAruninfo.txt",
 path = system.file(package = 'qpToolkit','NONMEM')
)
process.runrec(rr)
process.runrec(rr, plain = TRUE)
}
\seealso{
\code{\link{read.runrec}}, \code{\link{process.parTable}}
}
