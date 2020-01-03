# name:     conDataFun2
# purpose:  creates a summary of median (min -max) of input 
# input:    numeric vector
# output:   character vector
# note:  

# ROXYGEN Documentation
#' Summary: median (min - max)
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},   \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabSummarize}} 
#' @export
#' @examples 
#' x=rnorm(1000)
#' conDataFun2(x, 3)

conDataFun2 = function(y, digits = 3) 
  paste(signif(median(y),digits)," (", signif(min(y),digits)," - ",signif(max(y),digits), ")",sep="")
