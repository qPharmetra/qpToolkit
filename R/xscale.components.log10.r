#' Labels 10-logged x axes appropriately in lattice
#' @param lim internal required argument. 
#' @param \dots passed to xscale.components.default()
#' @return Nothing. It's a function to be called inside a lattice call
#' @export
#' @family scale.components
#' @note Not to be called by user. 
#' @import lattice
#' @examples
#' library(lattice)
#' set.seed(1234)
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

xscale.components.log10 = function(lim, ...){
  ans = xscale.components.default(lim  = lim, ...)
  tick.at = logTicks(10^lim, loc = 1)
  ans$bottom$ticks$at = log(tick.at, 10)
  ans$bottom$labels$at = log(tick.at, 10)
  labs = as.character(tick.at)
  labs[grep("e", labs)]
  ans$bottom$labels$labels = parse(text = tick.at)
  ans
}
#' Labels 10-logged x axes appropriately in lattice (ticks at powers of 1 and 3)
#' @param lim internal required argument. 
#' @param \dots passed to xscale.components.default()
#' @return Nothing. It's a function to be called inside a lattice call
#' @export
#' @family scale.components
#' @note Not to be called by user. 
#' @import lattice
#' @examples
#' library(lattice)
#' set.seed(1234)
#' datf = data.frame(TIME = exp(rnorm(3000, sd=0.6)), DV = seq(3000))
#'
#' xyplot(DV ~ TIME
#'        , data = datf
#'        , scales = list(x = list(log = 10))
#'        , xscale.components = xscale.components.log10.3
#' )

xscale.components.log10.3 = function(lim, ...){
  ans = xscale.components.default(lim  = lim, ...)
  tick.at = logTicks(10^lim, loc = c(1,3))
  ans$bottom$ticks$at = log(tick.at, 10)
  ans$bottom$labels$at = log(tick.at, 10)
  labs = as.character(tick.at)
  labs[grep("e", labs)]
  ans$bottom$labels$labels = parse(text = tick.at)
  ans
}
