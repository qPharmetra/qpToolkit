
# ROXYGEN Documentation
#' Exchange Existing set of Values for another set of Values
#' @description Exchange Existing set of Values for another set of Values
#' @param x Vector to apply the exchange over
#' @param uniques (Sorted) vector of unique OLD values
#' @param newUniques (Sorted) vector of unique NEW values
#' @return Swapped vector of x 
#' @export
#' @examples
#' my.vect = c(1,2,2,2,4,4,4,3,3,3,3)
#' swap(my.vect,c(1,2,3,4),c(11,22,33,44))

swap = function(x, uniques, newUniques)
{
  if(length(uniques) != length(newUniques))
    stop("length(uniques) != length(newUniques)")
  tmp = match(x, uniques, 0)
  x[as.logical(tmp)] = newUniques[tmp]
  return(x)
}
#exchange.values(mData.grpd$dose,unique(mData.grpd$dose), paste(unique(mData.grpd$dose),"mg"))
#exchange.values(mData.grpd$trt, c("placebo","drug 100mg", "comp34"), c(0,100,34))

