# name:     conDataFun2
# purpose:  creates a summary of median (min -max) of input
# input:    numeric vector
# output:   character vector
# note:

# ROXYGEN Documentation
#' Summary: median (min - max)
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},   \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabSummarize}}
#' @export
#' @examples
#' x=rnorm(1000)
#' conDataFun2(x, 3)

conDataFun2 <- function(y, digits = 3, latex = FALSE, align.dot = FALSE)
  paste(
     formatted.signif(
        median(y),
        digits = digits,
        latex = latex,
        align.dot = align.dot
     ),
     " (",
     formatted.signif(
        min(y),
        digits = digits,
        latex = latex,
        align.dot = align.dot
     ),
     " - ",
     formatted.signif(
        max(y),
        digits = digits,
        latex = latex,
        align.dot = align.dot
     ),
     ")",
     sep=""
   )
