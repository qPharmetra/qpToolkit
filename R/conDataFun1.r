
# ROXYGEN Documentation
#' Summary: mean (SD)
#' @param y numeric vector to summarize
#' @param nSignif number of significant digits
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabStats}}
#' @export
#' @examples
#' x=rnorm(1000)
#' conDataFun1(x, 3)

conDataFun1 = function(y, nSignif) 
  paste(signif(mean(y),nSignif)," (",signif(sqrt(stats::var(y)),nSignif-1),")", sep="")  

