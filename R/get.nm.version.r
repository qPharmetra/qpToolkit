
# ROXYGEN Documentation
#' NONMEM version used
#' @description Get NONMEM version (7.3.0 / 7.2.x etc )
#' @param run run rootname (e.g. run1)
#' @param path = directory where NONMEM run resides
#' @param file.ext NONMEM output file extension (defaults to .ext)
#' @return character string with NONMEM version used
#' @export
#' @seealso \code{\link{read.out}}
#' @examples
#' get.nm.version(run = "example1", path = getOption("qpExampleDir"))

get.nm.version = function(run, path = getOption("nmDir"), file.ext = ".lst")
{
  out = read.out(path=path, run=run, file.ext=file.ext)
  version = if(any(grepl("VERSION", out))) {
    nmversion =  out[grep("VERSION", out)][1]
    nmversion = substring(nmversion, regexpr("VERSION", nmversion)[1]+8)
    return(nmversion)
  }
}

## make an alias
nm.version = get.nm.version



