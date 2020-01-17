

# ROXYGEN Documentation
#' Scan NONMEM output file
#' @description Scan and parse the NONMEM control stream (mod file)
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext file extension of the NONMEM output file (.mod)
#' @param quiet if TRUE no message will be returned.
#' @return character vector with the NONMEM output. Each element i the vector represents a line in the NONMEM output file
#' @export
#' @seealso \code{\link{read.ext}}, \code{\link{read.out}}
#' @examples
#' head(read.mod(path = getOption("qpExampleDir"), run = "example1", file.ext = ".ctl"))

read.mod <- function(run, path = getOption("nmDir"), file.ext = ".mod", quiet = TRUE)
{
   theRun = paste(run,gsub("[.]","",file.ext), sep = ".")
   ctl = scan(file = file.path(path,theRun), what = "character", sep = "\n", quiet = quiet)
   return(ctl)
}
