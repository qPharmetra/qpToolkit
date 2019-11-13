#' Labels 10-logged y axes appropriately in lattice
#' @param lim internal required argument. 
#' @param \dots passed to yscale.components.default()
#' @return Nothing. It's a function to be called inside a lattice call
#' @export
#' @family scale.components
#' @note Not to be called by user.
#' @import lattice
#' @examples 
#' set.seed(1234)
#' library(lattice)
#' datf = data.frame(DV = exp(rlnorm(1000, sdlog=0.5)), TIME = seq(1000))
#' 
#' # labels at one log10 unit intervals
#' xyplot(DV ~ TIME
#'        , data = datf
#'        , scales = list(log = 10)
#'        , yscale.components = yscale.components.log10
#'        , xscale.components = xscale.components.log10
#' )
#' 
#' # labels at half log10 unit intervals
#' xyplot(DV ~ TIME
#'        , data = datf
#'        , scales = list(log = 10)
#'        , yscale.components = yscale.components.log10.3
#'        , xscale.components = xscale.components.log10.3
#' )

yscale.components.log10 = function (lim, ...){
  ans = yscale.components.default(lim = lim, ...)
  tick.at = logTicks(10^lim, loc = 1)
  ans$left$ticks$at = log(tick.at, 10)
  ans$left$labels$at = log(tick.at, 10)
  labs = as.character(tick.at)
  ans$left$labels$labels = parse(text = tick.at)
  ans
}
#' Labels 10-logged y axes appropriately in lattice (ticks at powers of 1 and 3)
#' @param lim internal required argument. 
#' @param \dots passed to yscale.components.default()
#' @return Nothing. It's a function to be called inside a lattice call
#' @export yscale.components.log10 yscale.components.log10.3
#' @family scale.components
#' @note Not to be called by user.
#' @import lattice
#' @export

yscale.components.log10.3 = function (lim, ...){
  ans = yscale.components.default(lim = lim, ...)
  tick.at = logTicks(10^lim, loc = c(1,3))
  ans$left$ticks$at = log(tick.at, 10)
  ans$left$labels$at = log(tick.at, 10)
  labs = as.character(tick.at)
  ans$left$labels$labels = parse(text = tick.at)
  ans
}

