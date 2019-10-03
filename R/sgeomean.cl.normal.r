
# ROXYGEN Documentation
#' Summary of geometric mean and 95\% CI 
#' @description Make a summary of the geometric mean and 95\% CI of the input
#' @param x numeric vector
#' @param conf.int confidence interval, default is 0.95, for 95\%)
#' @param log.base base of log, defaults to natural log
#' @param na.rm logical determining if NA values need to be omitted before the operation
#' @return Vector of three values, representing the geometric mean, and the lower and  upper component of the confidence interval.
#' @export
#' @examples
#' library(Hmisc)
#' test.vector = rlnorm(1000,log(100),0.5)
#' hist(test.vector)
#' smean.cl.normal(test.vector) ## arithmetic mean and 95% confidence intervals
#' sgeomean.cl.normal(test.vector) ## geometic mean and 95% confidence intervals

sgeomean.cl.normal = function(x, conf.int = 0.95, log.base=exp(1),
                              na.rm = TRUE)
{
  if (na.rm) 
    x <- log(x[!is.na(x)], base = log.base)
  n <- length(x)
  if(n > 1){
    xbar <- sum(x)/n
    se <- sqrt(sum((x - xbar)^2)/n/(n - 1))
    mult = qt((1 + conf.int)/2, n - 1) 
  } else{
    xbar = xbar = sum(x)/2
    se = 0
    mult = 0
  }
  se[is.na(se)] = 0
  c(Mean = exp(xbar), Lower = exp(xbar - mult * se), Upper = exp(xbar + mult * se), N=n)
}

