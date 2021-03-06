% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/png.report.r
\name{png.report}
\alias{png.report}
\title{PNG graph for reports generation}
\usage{
png.report(
  filename,
  w = 6,
  h = 4,
  units = "in",
  psize = 10,
  bg = "transparent",
  res = 600,
  ...
)
}
\arguments{
\item{filename}{filename including path. To follow qP workflow standards should read the internal relative reference directory "../WorkAre/output/graphs"}

\item{w}{width in \code{units}}

\item{h}{height in \code{units}}

\item{units}{units ('in', 'cm', or 'pt')}

\item{psize}{pointsize in \code{units}}

\item{bg}{background color}

\item{res}{resolution of PNG graph. High values create top quality graph at the expense of large file size.}

\item{...}{any additional parameters to be passed on the \code{png()}}
}
\value{
A graph written to file in .png format that can be included into reports, for example using LaTeX typesetting.
}
\description{
Writes graph to file for inclusion in report. The function opens a graphics device, after which the plot call(s) should follow. To be executed, the graphics device needs to be closed with \code{dev.off()}.
}
\examples{
\dontrun{ png.report(filename = "test.png") }
set.seed(1234567)
plot(x = 1:25,y = rnorm(25,50,5))
dev.off()
}
\seealso{
\code{\link{png.slide.semi}}, \code{\link{png.slide.full}}
}
