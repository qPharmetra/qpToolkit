#' Summary: median (min - max)
#' 
#' Creates a summary of median (min -max) of input.
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @param na.rm passed to \code{\link{median}}, \code{\link{min}}, \code{\link{max}}
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},   \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabSummarize}}
#' @export
#' @return character
#' @examples
#' x = rnorm(1000)
#' conDataFun2(x, 3)

conDataFun2 <- function(y, digits = 3, latex = FALSE, align.dot = FALSE, na.rm = FALSE)
  paste(
     formatted.signif(
        median(y, na.rm = na.rm),
        digits = digits,
        latex = latex,
        align.dot = align.dot
     ),
     " (",
     formatted.signif(
        min(y, na.rm = na.rm),
        digits = digits,
        latex = latex,
        align.dot = align.dot
     ),
     " - ",
     formatted.signif(
        max(y, na.rm = na.rm),
        digits = digits,
        latex = latex,
        align.dot = align.dot
     ),
     ")",
     sep = ""
   )
