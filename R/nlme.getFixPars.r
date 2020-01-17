# name:     nlme.getFixPars
# purpose:  extracts fixed effect point estimate and var-cvo matrix
# input:    nlme object
# output:   list with fixed effect point estimate and var-cvo matrix
# note:     called by nlme.simPars, nlme.extract

# ROXYGEN Documentation
#' NLME fixed effects and covariance matrix
#' @description extracts fixed effect point estimates and variance-covariance matrix of an nlme object
#' @param obj an nlme object
#' @return list with fixed effect point estimates and variance-covariance matrix
#' @export nlme.getFixPars
#' @seealso \code{\link{nlme.getRanPars}}, \code{\link{nlme.simPars}}, \code{\link{nlme.extract}}
#' @importFrom nlme fixef
#' @examples
#' library(nlme)
#' fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
#'   data = Loblolly,
#'   fixed = Asym + R0 + lrc ~ 1,
#'   random = Asym ~ 1,
#'   start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
#'   )
#' nlme.getFixPars(fm1)

nlme.getFixPars <- function(obj)
{
  val = list(coef = fixef(obj), var = obj$varFix)
  return(val)
}
