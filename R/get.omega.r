# ROXYGEN Documentation
#' Summary: OMEGA estimates
#' @description  Subset parameter estimate table by OMEGAs
#' @param run run rootname (e.g. 'run1')
#' @param path directory where rootname.ext resides
#' @param ... additional arguments passed to \code{nm.params.table}.
#' @return data frame with OMEGA estimates, coefficient of variation, standard error, and estimated/fixed information.
#' @export
#' @seealso \code{\link{nm.params.table}}, \code{\link{get.theta}}, \code{\link{get.sigma}}
#' @examples
#' get.omega("example2",  path = getOption("qpExampleDir"), fixed.text = "(fixed to 0)")
get.omega=function(run, path=getOption("nmDir"), ...)
{
   partab=nm.params.table(run=run, path=path, ...)
   subset(partab,grepl("OMEGA",Parameter))
}

