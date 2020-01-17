# name:     read.out
# purpose:  parse the NONMEM output (lst file) as scanned lines
# input:    run number, file.path and file extension of NONMEM output file
# output:   character vector with 1 entry per line of the NONMEM output file
# note:

# ROXYGEN Documentation
#' Scan NONMEM output file
#' @description Scan and parse the NONMEM output (lst file) as scanned lines. This function was tested for NONMEM v7.2 and v7.3.
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext file extension of the NONMEM output file (.lst)
#' @param quiet if TRUE no message will be returned.
#' @return character vector with the NONMEM output. Each element i the vector represents a line in the NONMEM output file
#' @export
#' @seealso \code{\link{read.ext}} \code{\link{get.ofv}}, \code{\link{get.dOFV}}, \code{\link{get.eigen}}, \code{\link{get.shrinkage}}, \code{\link{get.description}}
#' @examples
#' head(read.out(path=getOption("qpExampleDir"), run="example1", file.ext = ".lst"))

read.out <- function(run, path = getOption("nmDir"), file.ext = ".lst", quiet = TRUE)
{
  outName = paste(path, paste(run, file.ext, sep=""), sep="/")
  out = invisible(scan(file = outName, what="character", sep="\n", quiet = quiet))
  return(out)
}
