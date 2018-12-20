# name: panel.superpose.arrows.r
# purpose: a panel function that can be used with xyplot() (lattice) and related functions
#       to plot arrows connecting consecutive points. Useful for examining data for
#       hysteresis and proteresis.
# input: x and y coordinates to plot, subscripts and groups, and any other arguments
#       provided to xyplot per lattice xyplot conventions.
# output: When used within a lattice function, produces plot with arrows connecting 
#       consecutive points.

# ROXYGEN Documentation
#' Panel function for hysteresis plots
#' @description Panel function for hysteresis plots grouped by individual. The function is basically the arrow analogue of panel.superpose.
#' @param x,y numeric vectors x and y
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @examples
#' pat.1.avg.cdin=c(.1,.3,.45,.6,.7,.7,.6,.45,.3,.1)
#' pat.1.avg.cp.ipred=c(1,2,3,4,5,4.5,3.5,2.5,1.5,1)
#' pat.2.avg.cdin=c(.1,.3,.45,.6,.7,.7,.6,.45,.3,.1) + .2
#' pat.2.avg.cp.ipred=c(1,2,3,4,5,4.5,3.5,2.5,1.5,1)
#' plot.ds = data.frame(
#'   id = rep(c(1,2),each=10), 
#'   avg.cdin = c(pat.1.avg.cdin,pat.2.avg.cdin), 
#'   avg.cp.ipred = c(pat.1.avg.cp.ipred,pat.2.avg.cp.ipred)
#' )
#' trellis.strip.color()
#' plot(xyplot(avg.cdin*100 ~ avg.cp.ipred,
#'             plot.ds,
#'             groups = id,
#'             panel = panel.superpose.arrows,
#'             ylim = c(-25,115),
#'             scales=list(x=list(relation="free")),
#'             as.table = T,
#'             xlab = list("Mean of Indiv Predicted Drug Concentration (nmol/L)", cex=1.2),
#' ylab = list("Mean Thingy Inhibition (%)", cex=1.2)
#'))

panel.superpose.arrows = function(x,y, subscripts, groups,...)
{
  for(i in unique(groups))
  {
    xx = x[groups == i]
    yy = y[groups == i]
    s = seq(length(xx)-1)# 
    larrows(x0 = xx[s], y0 = yy[s], x1 = xx[s+1], y1 = yy[s+1], col = i)
  }
}

# Example


