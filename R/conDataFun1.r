
# ROXYGEN Documentation
#' Summary: mean (SD)
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabStats}}
#' @export
#' @examples
#' x=rnorm(1000)
#' conDataFun1(x, 3)

conDataFun1 = function(y, digits = 3) 
  paste(signif(mean(y),digits)," (",signif(sqrt(var(y)),digits-1),")", sep="")  

