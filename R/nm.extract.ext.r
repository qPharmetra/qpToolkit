
# ROXYGEN Documentation
#' NONMEM output extraction using .lst and .ext files
#' @description Extract NONMEM output based on the .ext output file
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @return A list with the NONMEM version, OFV, the xpose tables and the parameter table
#' @export
#' @seealso \code{\link{nm.extract.xml}}, \code{\link{nm.params.table}}, \code{\link{get.nm.version}}, \code{\link{get.ofv}}, \code{\link{get.xpose.tables}}
#' @import Hmisc
#' @examples 
#' run2.ext = nm.extract.ext("example2", path = getOption("qpExampleDir"))
#' run2.ext$version; run2.ext$final_objective_function

nm.extract.ext = function(run, path = getOption("nmDir"))
{
  version = get.nm.version(run = run, path = path)
  parTable = nm.params.table(run=run, path = path)
  final_objective_function = get.ofv(run = run, path = path)
  tables = get.xpose.tables(run = run, path = path)
  return(list(table=parTable,
    final_objective_function=final_objective_function,
    XPtable = tables, version = version)
  )
}

