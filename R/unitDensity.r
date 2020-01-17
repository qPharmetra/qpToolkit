
# ROXYGEN Documentation
#' Density plot normalized to unity
#' @description Create a density of inout and normalizes the maximum to be 1
#' @param x vector of numeric values to pass on to a \code{density}
#' @param ... any arguments to be passed on to \code{density default}
#' @return list of \code{x} and \code{y} with x and y information to generate a plot
#' @export
#' @importFrom stats density.default
#' @examples
#' standardNormal = unitDensity(qnorm(seq(0.0001,0.9999,length=1000),0,1))
#' plot(standardNormal$x, standardNormal$y, col = qp.blue, type = 'l')

unitDensity <- function(x,...){
   arg <- c(
      list(x = x),
      list(...)
   )
   arg <- arg[names(arg) %in% names(formals(density.default))]
   res <- do.call(density.default, arg)
   res$y <- with(res, y/max(y,na.rm=TRUE))
   res
}
