
# ROXYGEN Documentation
#' Panel function for Residual Variability Plots
#' @description Panel function to plot residuals vs any covariate or DV vs (I)PRED
#' @param x,y numeric vectors x and y
#' @param abline if "identity" (default) the line of identity is drawn. If anything else no abline is drawn.
#' @param col symbol color (defaults to obs.color)
#' @param cex symbol size
#' @param span passed to panel.loess()
#' @param smooth.col color of smooth line
#' @param lwd.loess line width of loess smooth
#' @param \dots passed to panel.xyplot(), panel.abline(), and panel.loess()
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @importFrom reshape2 melt
#' @examples
#' library(lattice)
#' library(reshape2)
#' trellis.strip.color()
#' xyplot(CONC ~ value | variable
#'        , data = subset(
#'           reshape2::melt(
#'              get.xpose.tables('example1', getOption('qpExampleDir')
#'              )
#'              , measure.vars=c('PRED','IPRED')
#'           ), EVID == 0)
#'        , subset = EVID==0
#'        , aspect = 1
#'        , scales = list(x = list(relation = 'free'))
#'        , panel = panel.residual
#'        )
#'
#'
panel.residual <- function(x, y
                          , abline="identity"
                          , col = obs.color
                          , cex = 1
                          , span = 0.8
                          , lwd.loess=2
                          , smooth.col = smooth.color
                          , ...
                          )
{
   panel.xyplot(x,y, cex=cex, col=col, ...)
   if(abline=="identity") panel.abline(a=0,b=1,col=loi.color, ...)
   if(length(x)>10) {panel.loess(x,y,col=smooth.col,lwd=lwd.loess,span=span,...)}
}

