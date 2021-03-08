#' Summary: mean (SD)
#' @param y numeric vector to summarize
#' @param digits number of significant digits
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @param na.rm passed to \code{\link{mean}}, \code{\link{var}}
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{catDataFun}}, \code{\link{tabStats}}, \code{\link{tabStats}}
#' @export
#' @examples
#' x = rnorm(1000)
#' conDataFun1(x, 3)
conDataFun1 <- function(y, digits = 3, latex = FALSE, align.dot = FALSE, na.rm = FALSE )
  paste(
     formatted.signif(
        mean(y, na.rm = na.rm),
        digits = digits,
        latex = latex,
        align.dot = align.dot
       ),
     " (",
     formatted.signif(
        sqrt(var(y, na.rm = na.rm)),
        digits = digits-1,
        latex = latex,
        align.dot = align.dot
      ),
     ")",
     sep = ""
    )


