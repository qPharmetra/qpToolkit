# name:     remove.tex.files
# purpose:  strips report area of latent tex files that should ideally be recreated every latex run
# input:    root name of latex files (character) and path where the LateX files reside
# output:   none
# note:

## strips the latex work area of latent files that should be recreated every time when rendering a report/presentation
## not removing you run the risk that old, non-applicable,  files are used

# ROXYGEN Documentation
#' Remove intermediate LaTeX Files
#' @description Strips report area of latent tex files that should ideally be recreated every latex run
#' @param name rootname of the LaTeX files.
#' @param path the report folder where intermediate LaTeX files reside
#' @return Intermediate LaTeX files are deleted - no visible R result.
#' @export

remove.tex.files <- function(name = "qPharmetra", path)
{
  #require(Hmisc)
  theFiles = paste(path,
                   paste(name, c('aux','bbl','blg','toc','log','lot','lof','out'),sep = "."), sep = "/")
  sapply(theFiles, function(x){if(exists(x)) invisible(file.remove(x))})
}


