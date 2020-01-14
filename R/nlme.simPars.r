
# ROXYGEN Documentation
#' Simulate from an nlme object
#' @description Draw samples from multivariate normal parameter distribution from an nlme object
#' @param paramList output from nlme.FixPars or nlme.RanPars
#' @param N sample size of the requested predictions
#' @return Matrix with parameter prediction
#' @export nlme.simPars
#' @importFrom MASS mvrnorm
#' @seealso \code{\link{nlme.getFixPars}}, \code{\link{nlme.getRanPars}}, \code{\link{nlme.extract}}
#' @examples
#' library(nlme)
#' fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
#'   data = Loblolly,
#'   fixed = Asym + R0 + lrc ~ 1,
#'   random = Asym ~ 1,
#'   start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
#'   )
#' nlme.getFixPars(fm1)
#' nlme.getRanPars(fm1)
#' nlme.simPars(nlme.getFixPars(fm1), N = 10)

nlme.simPars = function(paramList, N = 1)
{
  val = MASS::mvrnorm(n = N, mu = paramList$coef, Sigma = paramList$var)
  return(val)
}
