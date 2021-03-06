% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set.trellis.colors.r
\name{set.trellis.colors}
\alias{set.trellis.colors}
\title{Set Trellis/Lattice colors}
\usage{
set.trellis.colors(
  symbol = list(fill = gray[2], col = gray[6]),
  line = list(col = gray[7])
)
}
\arguments{
\item{symbol}{a list with colors for the strips}

\item{line}{a list with col(or) specified}
}
\value{
Lattice panel output (invisible)
}
\description{
when called before a lattice call, the strip panels will be set in grayscales
}
\examples{
library(lattice)
df = example.pkpdData()
xyplot(value~ time | format(dose) * type, data = df)
set.trellis.colors ()
xyplot(value~ time | format(dose), data = df, groups = type)
}
