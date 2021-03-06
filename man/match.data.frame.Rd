% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.data.frame.r
\name{match.data.frame}
\alias{match.data.frame}
\title{Compare two data frames}
\usage{
match.data.frame(x, table, nomatch = NA, incomparables = FALSE)
}
\arguments{
\item{x}{data frame 1 for comparison}

\item{table}{data frame 2 for comparison}

\item{nomatch}{exactly like the agurment nomatch in \code{match}}

\item{incomparables}{exactly like the argument incomparables in \code{match}}
}
\value{
Placeholder for return description
}
\description{
Compare two data frames (or lists) and provide feedback on what if any rows
}
\examples{
temp1 = data.frame(a = 11:13,b = 21:23)
temp2 = data.frame(a = 11:13,b = 21:23)
temp3 = data.frame(a = 11:13,b = 41:43)
temp4 = data.frame(a = 11:13,b = c(21,42,23))

match.data.frame(x = temp1, table = temp2)
match.data.frame(x = temp1, table = temp3)
match.data.frame(x = temp1, table = temp4)
#[1] 1 2 3
#[1] NA NA NA
#[1]  1 NA  3

match.data.frame(x = temp1, table = temp4, nomatch = 18)
#[1]  1 18  3

temp1 = list(x = 11:13)
temp2 = list(x = 11:13)
match.data.frame(x = temp1, table = temp2)
# 1 2 3

temp1 = list(x = cars[,1], y = cars[,2])
temp2 = list(x = cars[,1], y = cars[,2])
match.data.frame(x = temp1, table = temp2)
#[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 17 19 20 21 22 23 24
#25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50

temp1 = data.frame(a = 11:13,b = 21:23)
temp2 = data.frame(a = 11:14,b = 21:24)# dataframes with different numbers of rows

match.data.frame(x = temp1, table = temp2)
#[1] 1 2 3


}
