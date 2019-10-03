# name:       extract.varcov
# purpose:    get the covariance matrix from an already parsed .cov output file
# input:      the name of the NONMEM run, which should be held in a same-named folder within
#             the standard qp NONMEM folder for a project
# output:     a named list. The sole item in the list should have a name that is the
#             estimation method used (e.g. "First Order Conditional Estimation with Interaction").
#             That named item will be a data frame containing the covariance matrix.
# note:       function not to be called alone - is called by qP function nm.covmat.extract inside get.covmat


# ROXYGEN Documentation
#' Get the covariance matrix from an already parsed .cov output file.
#' @section Notes:
#' function not to be called alone - is called by qP function nm.covmat.extract inside get.covmat
#' @param text A character or numeric string containing the .cov output.
#' @return A named list. The sole item in the list should have a name that is the estimation method 
#' used (e.g. "First Order Conditional Estimation with Interaction").  That named item will be a 
#' data frame containing the covariance matrix.
#' @importFrom Hmisc unPaste
#' @export extract.varcov
#' @seealso \code{\link{nm.covmat.extract}}, \code{\link{get.covmat}}
#' @examples
#' \dontrun{
#' nm.unzip(path = file.path(getOption("qpExampleDir"),"run11"),run = "run11",extension = ".cov")
#' covText   = scan(file = file.path(getOption("qpExampleDir"),"run11","run11.cov"), what = "character",sep = "\n", quiet = T)
#' 
#' ## the heart of the function: parse space delimited text into a matrix
#' cov = extract.varcov(covText[-1])
#' 
#' ## warning: this matrix is LARGE. View only start and end of output
#' head(cov)
#' tail(cov)
#'
#' file.remove(file.path(getOption("qpExampleDir"),"run11","run11.cov")) ## clean things up
#' }

extract.varcov = function(text)
{ 
  nams = substring(text[-1], 2, 13)
  covmat = lapply(lapply(substring(text[-1], 15), Hmisc::unPaste, sep = " "), 
                  function(x) as.numeric(unlist(x[!is.element(x, c("", " "))])))
  names(covmat) = nams
  covmat = as.data.frame(do.call("rbind", covmat))
  names(covmat) = nams
  covmat
}
