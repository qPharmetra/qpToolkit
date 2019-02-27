
# ROXYGEN Documentation
#' Unzip function for zipped PsN output
#' @description Many large filews like XML outpout and covariance matrices are zipped by (qPharmetra's
#'  tweaked version of) PsN. To enable the parsing function of the \code{nm.xxx} domain to
#'   operate well this function unzips these files.  The unzip call is pulled from the system environment
#'   variable UNZIP_CALL on startup, or defaulted as stated below.  the \%s in the call is replaced with the 
#'   basename (no extension) of the file to unzip.  The unzip call can be modified by changine the 
#'   option unzip.call.
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param extension file extension of the file to unzip. Could be ".xml", or ".cov", or ".cor" or anything else that has been zipped
#' @param zip.extension extension of the zip program. Defaults to ".7z"
#' @param zip.program full OS call to zip utility for unzipping. Defauls to "c:/progra~1/7-zip/7z e \%s.7z"
#' @param filename file name (excluding path) of the filename to extract. Is automatically populated with qP workflow defaults
#' @param zip.filename filename of the zip file to extract. Is automatically populated with qP workflow defaults
#' @return The zipped file is extracted to disk in the same location. Nothing is returned to R.
#' @export
#' @examples 
#' file.exists( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' nm.unzip(run="example1", extension=".cov", path = file.path(getOption("qpExampleDir"),"example1"),quiet=FALSE)
#' file.exists( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' file.remove( file.path(getOption("qpExampleDir"),"example1/example1.cov"))

nm.unzip = function(run
                    , path = file.path(getOption("nmDir"), run)
                    , zip.extension = NULL
                    , extension = ".xml"
                    , zip.call = getOption("unzip.call")
                    , filename = paste(run, extension,sep="")
                    , zip.filename = file.path(path,filename)
                    , quiet = TRUE
)
{
   currentwd = getwd()
   newwd = path
   if(!quiet) cat("path:", newwd, "\n")
   zip.call = sprintf(zip.call, zip.filename)
   if(!quiet) cat("call:",zip.call,"\n")
   setwd(newwd)
   invisible(system(zip.call, ignore.stdout=quiet, ignore.stderr = quiet))
   setwd(currentwd)
}