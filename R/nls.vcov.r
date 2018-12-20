# name:     nls.vcov
# purpose:  extracts the variance covariance matrix from an nls object
# input:    nls object 
# output:   (variance-covariance) matrix
# note: 

## extracts the variance covariance matrix from an nls object

# ROXYGEN Documentation
#' NLS variance covariance matrix
#' @description Extracts the variance covariance matrix from an nls object
#' @param object an nls object
#' @return Placeholder for return description
#' @export
#' @examples
#'   DNase1 <- subset(DNase, Run == 1)
#' fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
#' summary(fm1DNase1)
#' nls.vcov(fm1DNase1)

nls.vcov <- function(object)
{
  sm <- summary(object)
  sm$cov.unscaled * sm$sigma^2
}

