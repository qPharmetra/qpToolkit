# name: myPNG
# purpose:  wrapper function for a call to png(), which opens a plot device (i.e. enables you
#       to "print" a graphic to a file), and provides defaults for various parameters of that
#       graphic.
#
#       Note after calling myPNG, a plot must be generated and then dev.off() must be called to
#       close the plot device (i.e. close the output graphic file)
# input: file name including relative path to which to store the graphic, width, height, units,
#       pointsize, background, and resolution.
# output: when followed by a call to a plotting function and then dev.off(), yields a PNG file
#       containing the graphic.

# ROXYGEN Documentation
#' General PNG grpah creator
#' @description A general wrapper function for a call to png(), which opens a plot device (i.e. enables you to "print" a graphic to a file). Note that the plot must be generated and then \code{dev.off()} must be called to close the plot device (i.e. close the output graphic file)
#' @param filename filename including path. To follow qP workflow standards should read the internal relative reference directory "../WorkAre/output/graphs"
#' @param w width in \code{units}
#' @param h height in \code{units}
#' @param psize pointsize in \code{units}
#' @param units units ('in', 'cm', or 'pt')
#' @param bg background color
#' @param res resolution of PNG graph. High values create top quality graph at the expense of large file size.
#' @param ... any additional parameters to be passed on the \code{png()}
#' @return A graph written to file in .png format.
#' @export

myPNG = function(
  filename = "C:/Test/test.png", 
  w = 8, h = 6, units="in", 
  psize = 10, bg="transparent", res=600
  , ...)
{
  png(filename = filename, width = w, height = h, 
    units = units, pointsize = psize, bg = "white", res = res, ...)
}

# Example
if (F) {
  myPNG(filename = paste("output/graphs", "myPNGexample.png", sep="/"))
  
  plot(x=1:10,y=31:40)  
  
  dev.off()

}