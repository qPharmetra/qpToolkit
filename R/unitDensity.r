
# ROXYGEN Documentation
#' Density plot normalized to unity
#' @description Create a density of inout and normalizes the maximum to be 1
#' @param x vector of numeric values to pass on to a \code{density}
#' @param ... any arguments to be passed on to \code{density default}
#' @return list of \code{x} and \code{y} with x and y information to generate a plot
#' @importFrom metrumrg safe.call
#' @export
#' @importFrom stats density.default
#' @examples
#' standardNormal = unitDensity(qnorm(seq(0.0001,0.9999,length=1000),0,1))
#' plot(standardNormal$x, standardNormal$y, col = qp.blue, type = 'l')

unitDensity <- function(x,...){
  res <- safe.call(density.default,x=x,...)
  res$y <- with(res, y/max(y,na.rm=TRUE))
  res
}
