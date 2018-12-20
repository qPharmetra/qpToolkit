# name:     nlme.getRanPars
# purpose:  extracts random effect point estimate and var-cvo matrix
# input:    nlme object
# output:   list with random effect point estimate and var-cvo matrix
# note:     called by nlme.simPars, nlme.extract

# ROXYGEN Documentation
#' NLME random effects and covariance matrix
#' @description Extracts random effect point estimate and variance-covariance matrix of an nlme object
#' @param object an nlme object
#' @return list with fixed effect point estimates and variance-covariance matrix 
#' @export nlme.getRanPars
#' @seealso \code{\link{nlme.getFixPars}}, \code{\link{nlme.simPars}}, \code{\link{nlme.extract}}
#' @import nlme
#' @examples
#' library(nlme)
#' fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
#'   data = Loblolly,
#'   fixed = Asym + R0 + lrc ~ 1,
#'   random = Asym ~ 1,
#'   start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
#'   )
#' nlme.getFixPars(fm1)

nlme.getRanPars = function(object)
  {
    aux = object$apVar
    if(!is.numeric(aux)) stop(aux)
    val = list(coef = attr(aux, "Pars"))
    attr(aux, "Pars") = attr(aux, "natural") = attr(aux, "natUncons") = NULL
    val$var = aux
    return(val)
  }

if(F)
{
  fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
              data = Loblolly,
              fixed = Asym + R0 + lrc ~ 1,
              random = Asym ~ 1,
              start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
  nlme.getRanPars(fm1)
} 
