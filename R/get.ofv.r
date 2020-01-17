# name:     get.ofv
# purpose:  parse objective function value from NONMEM output file
# input:    run number, file.path and file extension of NONMEM output file
# output:   numeric double with OFV value
# note:

# ROXYGEN Documentation
#' Objective Function Value parser
#' @description Pull the Objective Function Value (OFV) from the NONMEM output file
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext file extension of the NONMEM output file (defaults to .lst)
#' @return named numeric value representing the OFV. The name is NONMEM's estimated-method-specific name for the OFV
#' @export
#' @seealso \code{\link{get.dOFV}} which calls \code{get.ofv} over multiple runs for a brief runrecord, \code{\link{read.out}}
#' @examples
#' get.ofv("example2", path = getOption("qpExampleDir"))
#' sapply(c("example1","example2"), get.ofv, path = getOption("qpExampleDir"))

get.ofv <- function(run, path = getOption("nmDir"), file.ext = ".lst")
{
  out = read.out(path = path, run = run, file.ext = file.ext)
  parName = out[grep("#OBJT", out)]
  parName = sub("#OBJT:","", parName)
  parName = sub("[*]+","", parName)
  parName = sub("[*]+","", parName)
  parName = trimSpace(parName)
  parValue = extract.number(out[grep("#OBJV", out)])
  names(parValue) = parName
  return(parValue)
}

if(F)
{
  get.ofv("run116", path = "C:/Projects/Software/R-utils/trunk/TestArea/NONMEM")
  get.ofv("example2", path = getOption("qpExampleDir"))
  sapply(c("run116","run118","run11"), get.ofv)
}
