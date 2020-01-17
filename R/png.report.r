
# ROXYGEN Documentation
#' PNG graph for reports generation
#' @description Writes graph to file for inclusion in report. The function opens a graphics device, after which the plot call(s) should follow. To be executed, the graphics device needs to be closed with \code{dev.off()}.
#' @param filename filename including path. To follow qP workflow standards should read the internal relative reference directory "../WorkAre/output/graphs"
#' @param w width in \code{units}
#' @param h height in \code{units}
#' @param units units ('in', 'cm', or 'pt')
#' @param psize pointsize in \code{units}
#' @param bg background color
#' @param res resolution of PNG graph. High values create top quality graph at the expense of large file size.
#' @param ... any additional parameters to be passed on the \code{png()}
#' @return A graph written to file in .png format that can be included into reports, for example using LaTeX typesetting.
#' @export
#' @importFrom grDevices png
#' @seealso \code{\link{png.slide.semi}}, \code{\link{png.slide.full}}
#' @examples
#'\dontrun{ png.report(filename="test.png") }
#' set.seed(1234567)
#' plot(x=1:25,y=rnorm(25,50,5))
#' dev.off()

png.report <- function(
  filename,
  w = 6, h = 4, units="in",
  psize = 10, bg="transparent", res=600, ...)
{
   if(missing(filename)) stop("filename must be provided.")
  png(filename = filename, width = w, height = h,
      units = units, pointsize = psize, bg = bg, res = res, ...)
}
