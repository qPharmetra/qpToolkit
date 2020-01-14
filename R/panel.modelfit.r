globalVariables(c(
   'PI.ci.down.arcol', 'PI.ci.med.arcol', 'PI.ci.up.arcol',
   'PI.real.down.col', 'PI.real.med.col', 'PI.real.up.col',
   'abline.color', 'abline.color.box', 'amber', 'apple',
   'blue', 'box.plot.color', 'brown', 'cobalt', 'crimson',
   'cyan', 'emerald', 'gray', 'green', 'histogram.color',
   'indigo', 'ipred.color', 'ketchup', 'lime', 'lin.fit.col',
   'loi.color', 'magenta', 'mauve', 'obs.color', 'olive',
   'orange', 'pink', 'pred.color', 'purple', 'qp.colors',
   'qp.colors.sorted', 'qp.green', 'red','smooth.ci.color',
   'smooth.color', 'steel', 'taupe', 'teal', 'violet',
   'yellow','qp.blue'
))

#' Panel function for Residual Variability Plots
#' @description Panel function to plot DV, PRED, IPRED overlays for individual profiles
#' @param x,y numeric vectors x and y
#' @param groups see \code{\link[lattice]{xyplot}}
#' @param subscripts see \code{\link[lattice]{xyplot}}
#' @param logX log the x-axis (defaults to F)
#' @param logY log the y-axis (defaults to F)
#' @param type.obs line type for observed data (defaults to 'b' for both lines and points)
#' @param pred.lty line type for population predictions
#' @param ipred.lty line type for individual predictions
#' @param \dots passed to llines()
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

panel.modelfit = function(
   x, y, groups, subscripts,
   logX = FALSE, logY = FALSE, type.obs = "b",
   pred.lty = 'solid', ipred.lty = 'solid', ...)
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
