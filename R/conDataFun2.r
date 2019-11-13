# name:     conDataFun2
# purpose:  creates a summary of median (min -max) of input 
# input:    numeric vector
# output:   character vector
# note:  

# ROXYGEN Documentation
#' Summary: median (min - max)
#' @param y numeric vector to summarize
#' @param nSignif number of significant digits
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},   \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabSummarize}} 
#' @export
#' @examples 
#' x=rnorm(1000)
#' conDataFun2(x, 3)

conDataFun2 = function(y, nSignif) 
  paste(signif(stats::median(y),nSignif)," (", signif(min(y),nSignif)," - ",signif(max(y),nSignif), ")",sep="")
