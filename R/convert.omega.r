# name:     convert.omega
# purpose:  converts omega names to match order in covariance and correlation matrix
# input:    named vector (with names being of the format 'OMEGA(x1,x2)')
# output:   reordered named vector
# note:     is called by qP function nm.extract.xml

# ROXYGEN Documentation
#' Omega Names Conversion
#' @description Convert omega names to match order in covariance and correlation matrix
#' @param x named vector (with names being of the format 'OMEGA(x1,x2)').
#' @return reordered named vector.
#' @export
#' @seealso \code{\link{convert.sigma}}
#' @importFrom Hmisc unPaste

convert.omega = function (x)
{
  ## cov and cor matrix have upper triangle
  ## omega and omegase have lower triangle
  ## this function converts omega and omegase in upper triangle to match with matrices
  X = names(x)
  X = Hmisc::unPaste(gsub(")", "", substring(X, 7)), ",")
  neworder = data.frame(value = x, names = names(x),x1 = as.numeric(X[[1]]),x2 = as.numeric(X[[2]]))
  #neworder$names[order(neworder$x2,neworder$x1), ]
  result = neworder$value[order(neworder$x1,neworder$x2)]
  names(result) = neworder$names[order(neworder$x1,neworder$x2)]
  return(result)
}

#' @example
#'  test = nm.extract.xml(run = "example1", path = getOption("qpExampleDir"))
#'  convert.omega(test$omega[[1]])
