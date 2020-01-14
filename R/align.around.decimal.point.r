# name:     align.around.decimal.point
# purpose:  takes numeric vector and returns (LATEX format) character vector enabling tabulation in LATEX with the values centered around the decimal point
# input:    numeric vector
# output:   character vector
# note:

# ROXYGEN Documentation
#' Padding numeric vector around decimal point
#' @description Pads a vector of numeric values so that the decimal is at the same ordinal location
#' @param x A vector of numbers.
#' @param sep The decimal separator.
#' @param len The total character width desired in the formatted field.
#' @return A character vector with members of length \code{len}, with \code{sep} located in the same position
#' of each member.
#' @export align.around.decimal.point
#' @usage align.around.decimal.point(x, sep = '\\\\.', len)
#' @examples
#' align.around.decimal.point(c(1,100, 0.01))

align.around.decimal.point  =
function(x, sep = "\\.", len)
{
  xx = unlist(lapply(lapply(paste(x), unPaste, sep = sep), function(x) x[[1]]))
  if(missing(len)) len = max(nchar(xx))
  sapply(seq(along=x), function(y, x, yy, len) paste(paste("$\\phantom{",
    paste(rep("0",(len-nchar(yy[y]))), collapse = ""), "}$", sep = ""),
    x[y], sep = ""), yy = xx, x = x, len = len)
}

# create short-hand alias
aadp = align.around.decimal.point


