# ROXYGEN Documentation
#' Correlation Matrix Extraction
#' @description Extract correlation matrix from a .cor file
#' @param run character string in the form of 'run1'
#' @param path Directory where .cov file resides
#' @return Covariate matrix
#' @export
#' @seealso \code{\link{extract.varcov}}, \code{\link{get.covmat}}, \code{\link{nm.covmat.extract}}
#' @importFrom Hmisc unPaste
#' @examples
#' nm.unzip(run="example1", extension=".cor", path = file.path(getOption("qpExampleDir"),"example1"))
#' run1.cormat =  nm.cormat.extract("example1", path = getOption("qpExampleDir"))
#' names(run1.cormat)
#' names(run1.cormat[[1]])
#' lapply(run1.cormat, dim)
#' file.remove( file.path(getOption("qpExampleDir"),"example1/example1.cor"))
#'
nm.cormat.extract <- function(run, path = getOption("nmDir"))
{
  internalDir = paste(path, run, run, sep = "/")
  corText   = scan(file = paste(internalDir,".cor", sep = ""),
                   what = "character",sep = "\n", quiet = TRUE)
  locs = grep("TABLE", substring(corText, 1,6))
  ## define locations for each varcov table in the text
  corNames = substring(unlist(unPaste(corText[locs], ":")[[2]]),2)
  int = unPaste(corText[locs], sep = ":")[[1]]
  int = gsub("[.]", "", int)
  int = gsub(" ", "", int)
  tabnums = extract.number(int)
  locs = c(locs, length(corText)); locations = list(NULL)
  for(i in 1:length(locs[-length(locs)])){
    locations[[i]] = corText[seq((locs[i]+1),(locs[i+1]))]}
  corText = lapply(locations,function(x) if(length(grep("TABLE", x))>0)
    x[-grep("TABLE", x)] else x)
  names(corText) = tabnums
  cormat = lapply(corText, extract.varcov)
  names(cormat) = corNames
  return(cormat)
}
