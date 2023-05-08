#' Unzip function for zipped PsN output
#' @description Many large files like XML output and covariance matrices are zipped by (qPharmetra's
#'  tweaked version of) PsN. To enable the parsing function of the \code{nm.xxx} domain to
#'  operate well this function unzips these files.  The unzip call is pulled from the system environment
#'  variable UNZIP_CALL on startup, or defaulted as stated below.  the \%s in the call is replaced with the
#'  basename (no extension) of the file to unzip.  The unzip call can be modified by changing the
#'  option unzip.call.  Paths are specified relative to the current directory.
#'  
#' @param run run rootname (e.g. \code{run1})
#' @param extension file extension of the file to unzip. Could be ".xml", or ".cov", or ".cor" or anything else that has been zipped
#' @param filename file name (excluding path) of the filename to extract. Is automatically populated with qP workflow defaults
#' @param path directory where \code{run} resides
#' @param tmpdir directory to use if temp is true
#' @param temp if true, unzip to a unique temporary file path
#' @param outdir location for unzipped file
#' @param zip.filename filename of the zip file to extract. Is automatically populated with qP workflow defaults
#' @param zip.call full OS call to zip utility for unzipping. Defaults to "c:/progra~1/7-zip/7z e \%s.7z" on windows. Option "-o" is currently appended by nm.unzip().
#' @param command zip.call with filename substituted; passes to system
#' @param target path for expected result; possibly identical to zip.filename
#' @param quiet whether to suppress process messages
#' @param ... ignored arguments
#' @return (invisible) path to the unzipped file: especially useful if temp is true.
#' @export
#' @examples
#' file.exists( file.path(getOption("qpExampleDir"),"example1"))
#' file.exists( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' nm.unzip(run = "example1", extension = ".cov",
#' path = file.path(getOption("qpExampleDir"),"example1"),quiet = FALSE)
#' file.exists( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' file.remove( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' file.exists('c:/progra~1/7-zip/7z.exe')
#' # If the above does not exist, consider:
#' # options(unzip.call = 'C:/my-zip-path/7z e %s.7z') # or similar
#' tmp <- nm.unzip(
#'   run = "example1", 
#'   extension = ".cov",
#'   path = file.path(
#'     getOption("qpExampleDir"),
#'     "example1"
#'   ),
#'  temp = TRUE
#' )
#' file.exists( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' file.exists(tmp)

nm.unzip <- function(
   run,
   extension = ".xml",
   filename = paste(run, extension, sep = ""),
   path = file.path(getOption('nmDir', ''), 
     run
   ),
   tmpdir = tempfile('subdir'),
   temp = FALSE,
   outdir = ifelse(temp, tmpdir, path),
   zip.filename = file.path(path, filename),
   zip.call = getOption("unzip.call"),
   command = paste0(
     sprintf(
       zip.call,
       gsub(' ', '\\\\ ', zip.filename)
     ),
     ' -o',
     gsub(' ', '\\\\ ', outdir)
   ),
   target = file.path(outdir, filename),
   quiet = TRUE,
   ...
)
{
   if(!quiet) cat("path:", path, "\n")
   if(!quiet) cat("file:", zip.filename, "\n")
   if(temp) dir.create(tmpdir)
   if(temp) stopifnot(file.exists(tmpdir))
   if(!quiet) cat("call:", command, "\n")
   res <- system(command, ignore.stdout = quiet, ignore.stderr = quiet)
   if(!file.exists(target)) stop('unzip failed: ', res)
   invisible(target)
}
