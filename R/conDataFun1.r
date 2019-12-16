
# ROXYGEN Documentation
#' Summary: mean (SD)
#' @param y numeric vector to summarize
#' @param nSignif number of significant digits
#' @param digits number of significant digits in output, alternative specification
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabStats}}
#' @export
#' @examples
#' x=rnorm(1000)
#' conDataFun1(x, 3)

conDataFun1 = function(y, nSignif, digits = nSignif, formatted = TRUE, latex = FALSE, align.dot = FALSE ) 
  paste(
     formatted.signif(
        mean(y),
        digits = digits, 
        latex = latex, 
        align.dot = align.dot
       ),
     " (",
     formatted.signif(
        sqrt(var(y)),
        digits = digits-1,
        latex = latex,
        align.dot = align.dot
      ),
     ")", 
     sep=""
    )  

