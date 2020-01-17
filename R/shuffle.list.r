# name:     shuffle.list
# purpose:  shuffles a multi-level list
# input:    multi-level list
# output:   shuffled multi-level list
# note:     is called by qP function nm.extract.xml

# ROXYGEN Documentation
#' Shuffle/reorder lists
#' @description Shuffles a multi-level list
#' @param x list
#' @return A multi-level list
#' @export
#' @note Is called by qP function nm.extract.xml
#' @examples
#' test = list(A=list(a=1,b=2,c=3), B = list(a=2,b=1,c=0))
#' shuffle.list(test)

shuffle.list <- function(x)
{
  ## perhaps add a check to match elements in the list
  if(!is.list(x)) {message("-- x is not a list. No shuffle performed.");return()}
  ##
  nlist = apply(t(do.call("rbind",x)), 1, as.list)
  names(nlist) = names(x[[1]])
  return(nlist)
}


