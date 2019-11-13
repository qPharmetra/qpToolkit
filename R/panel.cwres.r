# ROXYGEN Documentation
#' Lattice panel function for CWRES plots
#' @description Panel function to plot CWRES vs any covariate
#' @param x,y numeric vectors x and y
#' @param col symbol color (defaults to obs.color)
#' @param pch symbol
#' @param col.loess color of smooth line
#' @param col.line color of abline

#' @param ... any other arguments to be passed on to the lattice call
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @examples 
#' library(lattice)
#' library(reshape2)
#'  out = get.xpose.tables("example2", getOption("qpExampleDir"))
#'  trellis.strip.color()
#' xyplot(CWRES ~ value | variable
#'    , data = subset(reshape2::melt(out, measure.vars = c('PRED','TIME')),EVID==0)
#'    , aspect = 1
#'    , scales = list(x = list(relation = "free"))
#'    , panel = panel.cwres
#'    )
panel.cwres = function(x,y, ...
                       , col = obs.color
                       , col.line = abline.color
                       , pch = 1
                       , col.loess = smooth.color)
{
   panel.abline(h=c(-2,0,2), lty=c(2,1,2), col = col.line)
   panel.xyplot(x, y, ..., col = col, pch = pch)
   panel.loess(x,y,..., col = col.loess, lwd = 2)
}

