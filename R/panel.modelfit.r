# ROXYGEN Documentation
#' Panel function for Residual Variability Plots
#' @description Panel function to plot DV, PRED, IPRED overlays for individual profiles
#' @param x,y numeric vectors x and y
#' @param groups see \code{\link[lattice]{xyplot}}
#' @param subscripts see \code{\link[lattice]{xyplot}}
#' @param logX log the x-axis (defaults to F) 
#' @param logY log the y-axis (defaults to F)
#' @param type.obs line type for observed data (defaults to 'b' for both lines and points)
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @examples
#' library(Hmisc)
#' out = get.xpose.tables('example1',getOption('qpExampleDir'))
#' out$EVID=0
#' trellis.strip.color()
#' xyplot(Cbind(CONC,PRED,IPRED,EVID) ~ TIME
#'        , groups = ID
#'        , data = subset(out)
#'        , scales = list(x = list(relation = 'free'),y = list(log=10))
#'        , panel = panel.modelfit
#'        , logY = TRUE
#'        , yscale.components = yscale.components.log10
#' )

panel.modelfit = function(x, y, groups, subscripts, logX = FALSE, logY = FALSE, type.obs = "b", ...)
{
   for(i in unique(groups[subscripts]))   
   {
      ok = groups[subscripts] == i
      yy = attr(y, "other")
      okobs = ok & yy[, 3] %nin% c(1,2)
      llines(x[okobs], y[okobs], ..., col = obs.color, type = type.obs)
      llines(x[ok], if(logY) log10(yy[ok,1]) else yy[ok,1], col = pred.color, lty = pred.lty, lwd = 3)
      llines(x[ok], if(logY) log10(yy[ok,2]) else yy[ok,2], col = ipred.color, lty=ipred.lty, lwd = 1)
   }
}
