

# ROXYGEN Documentation
#' Remove trailing items from vector
#' @description Removes items from the end of a numeric vector that are equal to the value supplied
#' @param x numeric vector
#' @param val the trailing value to be removed
#' @return Numeric vector \code{x} without trailing \code{val}
#' @export
#' @examples
#' conc = c(1:30,0,0,0,0)
#' conc
#' removeTrailing(conc, val=0)

removeTrailing = function(x, val=0){
  xx = rep(1, length(x))
  xx[x==val] = 0
  if(!all(xx==0)){
    ind=1:length(x)
    last = max(ind[xx>0])
    return(x[1:last])
  } else return(x)
}


