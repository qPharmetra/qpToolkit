
# ROXYGEN Documentation
#' Reorder names in a data.frame
#' @description It can be tedious to reorder the columns in a data.frame. This function lets you move specific columns relative to some other named column.
#' @param x data.frame
#' @param who a character vector of column names to move, or a logical vector of length names(x), or a vector of indices
#' @param after the column after which to put who: may be character, integer, NA, or NULL
#' @return data.frame: a variant of x
#' @export reorder.names
#' @import Hmisc
#' @examples
#' testData = expand.grid(start=1,middle=LETTERS[1:3], end=c("K","L"))
#' reorder.names(testData, who = "start", after = "end")

reorder.names <- function (x, who, after = NA) 
{
  names(x) <- make.unique(names(x))
  who <- names(x[, who, drop = FALSE])
  nms <- names(x)[!names(x) %in% who]
  if (is.null(after)) 
    after <- length(nms)
  if (is.na(after)) 
    after <- 0
  if (length(after) == 0) 
    after <- length(nms)
  if (is.character(after)) 
    after <- match(after, nms, nomatch = 0)
  if (after < 0) 
    after <- length(nms)
  if (after > length(nms)) 
    after <- length(nms)
  nms <- append(nms, who, after = after)
  x[nms]
}