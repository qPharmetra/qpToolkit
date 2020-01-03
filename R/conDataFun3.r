
# ROXYGEN Documentation
#' Summary: (95\%.lower - 95\%.upper)
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},  \code{\link{conDataFun2}}, \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabSummarize}} 
#' @export
#' @examples 
#' x=rnorm(1000)
#' conDataFun3(x, 3)
#' 
conDataFun3 = function(y, digits = 3)
{
  se = sqrt(var(y))/sqrt(length(y)) 
  ci = qt(c(0.975, 0.025), length(y)) * se + mean(y)
  ci = ci[order(ci)]
  
  return(
    paste(signif(mean(y),digits),  " (", 
          signif(ci[1],digits-1),  " - ",
          signif(ci[2],digits-1),  ")",
          sep="")
  )
}

