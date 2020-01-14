
# ROXYGEN Documentation
#' Summary: (95\%.lower - 95\%.upper)
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},  \code{\link{conDataFun2}}, \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabSummarize}} 
#' @export
#' @examples 
#' x=rnorm(1000)
#' conDataFun3(x, 3)
#' 
conDataFun3 = function(y, digits = 3, latex = FALSE, align.dot = FALSE)
{
  se = sqrt(stats::var(y))/sqrt(length(y)) 
  ci = stats::qt(c(0.975, 0.025), length(y)) * se + mean(y)
  ci = ci[order(ci)]
  
  return(
    paste(
      formatted.signif(
        mean(y),
        digits = digits,
        latex = latex,
        align.dot = align.dot
      ),  
      " (", 
      formatted.signif(
        ci[1],
        digits = digits-1,
        latex = latex,
        align.dot = align.dot
      ),  
      " - ",
      formatted.signif(
        ci[2],
        digits = digits-1,
        latex = latex,
        align.dot = align.dot
      ),  
      ")",
      sep=""
    )
  )
}

