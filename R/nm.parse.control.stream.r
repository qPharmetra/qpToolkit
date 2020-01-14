# name:     nm.parse.control.stream
# purpose:  function to read NONMEM control stream and parse it out as named list
# input:    run number (character), optionally the path where the NM run resides
# output:   list with as many (named) element as there are $.... elements in the cotnrol stream,
# note:     depends on qP function nm.remove.section

# ROXYGEN Documentation
#' Parse NONMEM control stream
#' @description Function to read NONMEM control stream and parse it out as named list
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext file extension of NONMEM control stream (.mod)
#' @param quiet if TRUE no message will be returned.
#' @return A named list with all \code{$PK, $ERROR, $THETA} etc... elements of the control stream
#' @export
#' @importFrom Hmisc unPaste
#' @examples
#' nm.parse.control.stream("example1", path = getOption("qpExampleDir"), file.ext = ".ctl")
#'
nm.parse.control.stream = function(run
                                   , path = getOption("nmDir")
                                   , file.ext = ".mod"
                                   , quiet= TRUE
)
{
   ctl = read.mod(run=run, path=path, file.ext=file.ext, quiet=quiet)

   ctlSections = ctl[grep("\\$", ctl)]
   ctlSections = ctlSections[substring(gsub(" ","",ctlSections),1,1)!=";"]
   ctlSections = unlist(lapply(ctlSections, function(x) unPaste(x,sep = " ")[[1]]))
   ctlSections = substring(unique(ctlSections), 2)
   ctlSections = ctlSections[ctlSections!=""]
   nams = ctlSections

   ctlSections = lapply(ctlSections
                       , function(x, ctl) {
                          as.vector(
                             unlist(
                                invisible(nm.remove.section(x,ctl)[[2]])
                             ), "character")
                       }
                       , ctl = ctl
   )
   names(ctlSections) = nams
   return(ctlSections)
}



