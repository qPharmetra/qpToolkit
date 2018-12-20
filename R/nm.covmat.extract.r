# name:     nm.covmat.extract
# purpose:  extract covariance matri(x)(ces) from *.cov file
# input:    run number, path
# output:   list of matri(x)(ces)
# note:     as.cov file are zipped by PsN's execute, this may require calling nm.unzip first

# ROXYGEN Documentation
#' Covariance Matrix Extraction
#' @description Extract covariance matrix from a .cov file
#' @param run character string in the form of 'run1'
#' @param path Directory where .cov file resides
#' @return Covariate matrix
#' @export
#' @seealso \code{\link{extract.varcov}}, \code{\link{get.covmat}}, \code{\link{nm.cormat.extract}}
#' @import Hmisc
#' @examples
#' nm.unzip(run="example1", extension=".cov", path = file.path(getOption("qpExampleDir"),"example1"))
#' run1.covmat =  nm.covmat.extract("example1", path = getOption("qpExampleDir"))
#' names(run1.covmat)
#' names(run1.covmat[[1]])
#' lapply(run1.covmat, dim)
#' file.remove( file.path(getOption("qpExampleDir"),"example1/example1.cov"))
#' 

nm.covmat.extract = function(run, path = getOption("nmDir"))
{
  internalDir = paste(path, run,run,sep = "/")
  covText   = scan(file = paste(internalDir,".cov", sep = ""), 
    what = "character",sep = "\n", quiet = T)
  locs = grep("TABLE", substring(covText, 1,6))   
    ## define locations for each varcov table in the text
  covNames = substring(unlist(unPaste(covText[locs], ":")[[2]]),2)
  int = unPaste(covText[locs], sep = ":")[[1]]
  int = gsub("[.]", "", int)
  int = gsub(" ", "", int)
  tabnums = extract.number(int)
  locs = c(locs, length(covText)); locations = list(NULL)
  for(i in 1:length(locs[-length(locs)])){
    locations[[i]] = covText[seq((locs[i]+1),(locs[i+1]))]}
  covText = lapply(locations,function(x) if(length(grep("TABLE", x))>0) 
    x[-grep("TABLE", x)] else x)
  names(covText) = tabnums
  covmat = lapply(covText, extract.varcov)
  names(covmat) = covNames
  return(covmat)
}

 
