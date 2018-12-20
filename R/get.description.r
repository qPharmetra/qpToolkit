# ROXYGEN Documentation
#' Scan the runrecord tag for 'Description'
#' @description Scan the runrecord tag for 'Description'. This makes it easy to scan a few models to find some particular model
#' @param run run rootname (e.g. run1)
#' @param path directory where NONMEM output file resides
#' @param file.ext extension of the .ext file
#' @return The model's description tag as a character string
#' @export get.description
#' @seealso \code{\link{get.nm.version}}, \code{\link{get.ofv}}, \code{\link{read.out}}
#' @import Hmisc lattice
#' @examples
#' get.description(run = "example1", path = getOption("qpExampleDir"))

get.description = function(run, path = getOption("nmDir"), file.ext = ".lst")
{
  out = read.out(path=path, run=run, file.ext = file.ext)
  desc = substring(out[grep("Description:", out)+1],7)
  if(length(desc)==0) desc = "No Model Description"
  return(desc)
}

