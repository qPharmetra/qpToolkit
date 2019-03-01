# name: vec2df
# purpose: turns a vector into a data frame with n repeated instances of the vector. 
# input: a vector, and a number n of times to repeat the vector
# output: dataframe which has len(vector) columns, and repeats for n rows

# ROXYGEN Documentation
#' Turn vector into data.frame
#' @description Turns a vector into a data frame with n repeated instances of the vector.  
#' @param x vector
#' @param n number of times the vector needs to be repeated
#' @return data.frame. 
#' @note This function is used by \code{nm.covplot}
#' @export
#' @examples
#' vec2df(x=1:10,n=1)
#' vec2df(x=1:10,n=2)

vec2df = function(x, n = 1)
{
  xDF = data.frame(matrix(rep(x, n), ncol = length(x), byrow = TRUE))
  names(xDF) = names(x)
  return(xDF)
}
