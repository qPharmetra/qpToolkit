# name:     boxcoxf
# purpose:  transforms vector to Box-Cox transformation
# input:    numeric vector, 
# output:   numeric vector - boxcox tranformed input
# note:     



# ROXYGEN Documentation
#' Box-Cox Transformation
#' @description Apply a Box-Cox Transformation to a vector
#' @param shape The shape parameter.
#' @param distr The distribution parameter.
#' @return Box-Cox tranformation of \code{distr} with \code{shape}.
#' @export
#' @examples
#' x = rnorm(1000, mean = 0, sd = 0.25)
#' x.boxcox.pos = boxcoxf(0.5,x)
#' x.boxcox.neg = boxcoxf(-3.5,x)
#' par(mfrow=c(1,3))
#' hist(x)
#' hist(x.boxcox.pos)
#' hist(x.boxcox.neg)
boxcoxf = function(shape, distr){
  exp((exp(distr)**shape-1)/shape)
}
