
# ROXYGEN Documentation
#' Histogram normalized to unity
#' @description Plots a histogram of data provided in which the height of the frequency are normalized.
#' @param x numeric vector
#' @param myCuts alternative cut points (optional)
#' @param lwd.bar line width of the histogram bars
#' @param offset width between the bars
#' @param hist.type histogram type, can be 'box' or 'lines'
#' @param col.hist color of histograms
#' @param xlim limits of x coordinate
#' @param ylab label of the frequency (y label)
#' @param \dots passed to plot()
#' @return A histogram
#' @export
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics segments
#' @importFrom graphics polygon
#' @examples
#' set.seed(1234567)
#' myvals = rnorm(10000,50,5)
#' unitHistogram(myvals, xlab = "My Values", cex.lab = 1.25)


unitHistogram = function(x, myCuts = NULL, lwd.bar = 4, offset = 0.4, hist.type = "box",
  col.hist = "#D7D7D7", xlim = NULL, ylab = NULL, ...)
{
  ## create a histogram
  if(is.null(myCuts)) myCuts = seq(min(x),max(x))
  numeric.input = floor(as.numeric(as.character(cut3(x, cuts = myCuts, levels.mean = TRUE))))
  bar.lengths = table(numeric.input)
  bar.lengths = bar.lengths/max(bar.lengths)
  if(is.null(xlim)) xLimits = c((min(x) - offset), max(x))  else xLimits = xlim
  yLabel = if(is.null(ylab)) "normalized frequency"  else ylab

  plot(as.numeric(names(bar.lengths)), bar.lengths, type = 'n',  ylab = yLabel,
    ylim = c(0,1), xlim = xLimits, lwd = lwd.bar, ...)
  axis(2)
  xx = as.numeric(names(bar.lengths))
  for(i in 1 : length(xx))
  {
    if(hist.type == "lines")
    {
      segments(x0 = xx[i]-offset, x1 = xx[i]-offset, y0 = 0, y1 = bar.lengths[i], col = col.hist)
      segments(x0 = xx[i]+offset, x1 = xx[i]+offset, y0 = 0, y1 = bar.lengths[i], col = col.hist)
      segments(x0 = xx[i]-offset, x1 = xx[i]+offset, y0 = bar.lengths[i], y1 = bar.lengths[i], col = col.hist)
      segments(x0 = xx[i]-offset, x1 = xx[i]+offset, y0 = 0, y1 = 0, col = col.hist)
    } else {
      polygon(x = c(xx[i]+offset, xx[i]-offset, xx[i]-offset, xx[i]+offset),
              y = c(0,0, bar.lengths[i], bar.lengths[i]),
              col = col.hist, border = TRUE, angle = -1, density = -1)
    }

  }
}


