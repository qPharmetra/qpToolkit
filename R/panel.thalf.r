# name:     panel.thalf

# ROXYGEN Documentation
#' Panel function for elimination half life
#' @description Panel function to create individual plots with terminal half life predictions
#' @param x,y numeric vectors x and y
#' @param text.cex text font size
#' @param line.col line color
#' @param digits number of significant digits in output
#' @param unit time unit (hr)
#' @param ... any other arguments passed on to the lattice call
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @importFrom stats coef lm
#' @examples
#' library(lattice)
#' pkpdData = example.pkpdData()
#' pkpdData$blq = 0
#' pkpdData$blq[pkpdData$type=="PK"&pkpdData$value<0.1] = 1
#' xyplot(value ~ time | id
#' , data = subset(pkpdData, blq==0)
#' , subset = type == "PK" & dose == 100 & value > 1e-6
#' , groups = id
#' , panel = function(x,y, ...)
#' {
#'   panel.xyplot(x,y,..., col = "gray", pch = 18, type = "b", cex = 1)
#'   sel = length(x) 
#'   sel = seq(sel-3,sel)
#'   panel.thalf(x[sel],y[sel],...,lwd = 2)
#'   lpoints(x,y, col = steel)
#' }
#' , scales = list(y = list(log = 10))
#' , yscale.components = yscale.components.log10
#' )

panel.thalf = function(x, y, ...,
                       text.cex = 0.8,
                       line.col = "blue",
                       digits = 3,  ## number of significant digits
                       unit = "hr")
{
  # Draw a line segment covering the range of x for each panel
  if (length(x) > 2) ##  need at least 3 points for a credible half life
  {
    text.loc = c(current.panel.limits()[[1]][2],current.panel.limits()[[2]][2])
    coefs = coef(lm(as.numeric(y) ~ as.numeric(x)))
    xx=range(x)
    yy=coefs[1] + coefs[2]*xx
    panel.xyplot(xx, yy, type="l", ..., col = line.col)
    
    ## add half life as text to each panel
    coefs = coef(lm(y ~ x))
    thalf = signif(log10(2)/abs(coefs[2]), digits)
    len = nchar(thalf)
    panel.text(x = text.loc[1] * (0.72-0.02*len), y = text.loc[2]*0.95, "t", cex = text.cex)
    panel.xyplot(x = text.loc[1] * (0.76-0.02*len), y = text.loc[2]*0.95, pch = 189, cex = text.cex*1.2, col = 1)
    panel.text(x = text.loc[1]*(0.81-0.02*len), y = text.loc[2]*0.95,
               thalf, cex = text.cex, adj = 0)
    panel.text(x = text.loc[1]*0.99, y = text.loc[2]*0.95, unit, cex = text.cex, adj = 1)
  }
}


