globalVariables('Parameter')
# ROXYGEN Documentation
#' Summary: THETA estimates
#' @description  Subset parameter estimate table by THETAs
#' @param run run rootname (e.g. run1)
#' @param path directory where rootname.ext resides
#' @param ... additional arguments passed to \code{nm.params.table}.
#' @return data frame with THETA estimates, coefficient of variation, standard error, and estimated/fixed information.
#' @export
#' @seealso \code{\link{nm.params.table}}, \code{\link{get.omega}}, \code{\link{get.sigma}}
#' @examples
#' get.theta("example1",  path = getOption("qpExampleDir"), fixed.text = "(fixed to 0)")
get.theta <- function(run
                   , path=getOption("nmDir")
                   , ...
)
{
   partab <- nm.params.table(run=run, path=path, ...)
   subset(partab,grepl("THETA",Parameter))
}

