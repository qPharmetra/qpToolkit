% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reorder.names.r
\name{reorder.data.frame}
\alias{reorder.data.frame}
\title{Reorder names in a data.frame}
\usage{
\method{reorder}{data.frame}(x, ..., who, after = NA)
}
\arguments{
\item{x}{data.frame}

\item{\dots}{ignored}

\item{who}{a character vector of column names to move, or a logical vector of length names(x), or a vector of indices}

\item{after}{the column after which to put who: may be character, integer, NA, or NULL}
}
\value{
data.frame: a variant of x
}
\description{
It can be tedious to reorder the columns in a data.frame. This function lets you move specific columns relative to some other named column.
}
\examples{
testData = expand.grid(start = 1,middle = LETTERS[1:3], end = c("K","L"))
reorder(testData, who = "start", after = "end")
}
