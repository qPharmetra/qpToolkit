# name:     convert.sigma
# purpose:  converts sigma names to match order in covariance and correlation matrix
# input:    named vector (with names being of the format 'SIGMA(x1,x2)')
# output:   reordered named vector
# note:     is called by qP function nm.extract.xml

# ROXYGEN Documentation
#' Sigma Names Conversion
#' @description Convert sigma names to match order in covariance and correlation matrix
#' @param x named vector (with names being of the format 'SIGMA(x1,x2)').
#' @return reordered named vector.
#' @export
#' @seealso \code{\link{convert.omega}}
#' @importFrom Hmisc unPaste

convert.sigma <- function(x){
  ## cov and cor matrix have upper triangle
  ## sigma and sigmase have lower triangle
  ## this function converts sigma and sigmase in upper triangle to match with matrices
  X = names(x)
  X = Hmisc::unPaste(gsub(")", "", substring(X,7)), ",")
  neworder = order(X[[2]])
  names(x) = paste("SIGMA(",X[[2]], ",", X[[1]], ")", sep="")
  x[neworder]
}

if(F)
{
  test = nm.extract.xml("example1", path = "inst/NONMEM")
  names(test)
  convert.sigma(test$sigma[[1]])
}
