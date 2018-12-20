
# ROXYGEN Documentation
#' Plot QQ Gamma distribution
#' @description Plot qq-plot for gamma distributed variable
#' @param x numeric vector suspected to be Gamma distributed
#' @param ylab y label
#' @param xlab x label
#' @param main main title of the plot
#' @return character vector with the NONMEM output. Each element i the vector represents a line in the NONMEM output file
#' @export
#' @examples
#' qqGamma(rgamma(100,shape=0.1,scale = 0.2))
#' 
qqGamma = function(x
                  , ylab=deparse(substitute(x))
                  , xlab="Theoretical Quantiles"
                  , main="Gamma Distribution QQ Plot",...)
{
    # Plot qq-plot for gamma distributed variable

    xx = x[!is.na(x)]
    aa = (mean(xx))^2 / var(xx)
    ss = var(xx) / mean(xx)
    test = rgamma(length(xx), shape=aa, scale=ss)

    qqplot(test, xx, xlab=xlab, ylab=ylab, main=main,...)
    abline(0,1, lty=2)
}
