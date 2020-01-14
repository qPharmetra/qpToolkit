

# ROXYGEN Documentation
#' PNG graph for PowerPoint generation (full size)
#' @description A wrapper function for a call to png(), which opens a plot device (i.e. enables you to "print" a graphic to a file), and provides defaults for various parameters of that graphic suitable for a slide in a presentation. The plot area will over the full slide dimension and does not have to be resized or repositioned after pasting. Note that the plot must be generated and then \code{dev.off()} must be called to close the plot device (i.e. close the output graphic file)
#' @param filename filename including path. To follow qP workflow standards should read the internal relative reference directory "../WorkAre/output/graphs"
#' @param w width in \code{units}
#' @param h height in \code{units}
#' @param units units ('in', 'cm', or 'pt')
#' @param psize pointsize in \code{units}
#' @param bg background color
#' @param res resolution of PNG graph. High values create top quality graph at the expense of large file size.
#' @param ... any additional parameters to be passed on the \code{png()}
#' @return A graph written to file in .png format that does not have to be edited when pasted into MS PowerPoint(r).
#' @export
#' @importFrom grDevices png
#' @seealso \code{\link{png.slide.semi}}, \code{\link{png.report}}
#' @examples
#' png.slide.full(filename=file.path(tempdir(),"/myPNGslidefullexample.png"))
#' set.seed(1234567)
#' plot(x=1:25,y=rnorm(25,50,5))
#' dev.off()

png.slide.full = function(
  filename,
  w = 10, h = 7.5, units="in",
  psize = 10, bg="transparent", res=600, ...)
{
   if(missing(filename)) stop("filename must be provided.")
  png(filename = filename, width = w, height = h,
      units = units, pointsize = psize, bg = bg, res = res, ...)
}


