globalVariables('Parameter')
# ROXYGEN Documentation
#' Summary: SIGMA estimates
#' @description  Subset parameter estimate table by SIGMAs
#' @param run run rootname (e.g. 'run1')
#' @param path directory where rootname.ext resides
#' @param ... additional arguments passed to \code{nm.params.table}.
#' @return data frame with SIGMA estimates, coefficient of variation, standard error, and estimated/fixed information.
#' @export
#' @seealso \code{\link{nm.params.table}}, \code{\link{get.theta}}, \code{\link{get.omega}}
#' @importFrom metrumrg stableMerge
#' @examples
#' get.sigma("example2",  path = getOption("qpExampleDir"), fixed.text = "(fixed to 0)")
get.sigma=function(run, path=getOption("nmDir"), ...)
{
   partab=nm.params.table(run=run, path=path, ...)
   subset(partab,grepl("SIGMA",Parameter))
}

