# name: isMissing
# purpose: take a vector and return a vector of boolean values indicating which input
#       vector values are missing (NA).
# input: vector of numbers
# output: vector of boolean values indication which ones are missing (TRUE) or not
#       missing (FALSE)

# ROXYGEN Documentation
#' Check for missing values
#' @description Take a vector and return a vector of boolean values indicating which input vector values are missing (NA).
#' @param x vector
#' @return A vector of boolean values indicating which x is missing
#' @export
#' @seealso \code{\link{isNumeric}}, \code{\link{asNumeric}}
#' @examples
#' temp = data.frame(id=1:4, nom.time=c(5,NA,7,NA))
#' isMissing(temp$nom.time)
#' temp[isMissing(temp$nom.time),]

isMissing <- function(x) {
  natest = is.na(x)
  if(class(x)=="character") natest = natest|sub("^[ ]+$","",x)==""|x=="NA"
  natest
}
