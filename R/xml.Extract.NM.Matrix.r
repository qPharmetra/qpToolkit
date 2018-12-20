# name:     xml.Extract.NM.Matrix
# purpose:  extracts Matrix from a NONMEM xml matrix node
# input:    NONMEM xml matrix node
# output:   matrix (numeric)
# note:     is called by qP function nm.extract.xml

# ROXYGEN Documentation
#' XML parser for matrices
#' @description Extract XML content into a Numeric Matrix
#' @param y XML children
#' @seealso \code{\link{nm.extract.xml}}, \code{\link{make.symmetric}}
#' @return A matrix
#' @export xml.Extract.NM.Matrix
#' @import XML
#' @examples 
#' library(XML)
#' run = "example1"
#' filename = "C:/nm73g64/examples/example1.xml"
#' top = xmlRoot(xmlTreeParse(filename))# names(top)
#' TMP = top[["nonmem"]][["problem"]]
#' TMP = TMP[grepl("estimation", names(TMP))]
#' len = length(TMP)
#' ## get estimation titles
#' est.methods = sapply(1:length(TMP), function(x,TMP)
#'   xmlValue(TMP[x][["estimation"]][["estimation_title"]]), TMP = TMP)
#' names(TMP) = est.methods
#' tmp = TMP[[1]]
#' 
#' xml.Extract.NM.Value(asXMLNode(tmp[["theta"]]), what = "theta")
#' xml.Extract.NM.Value(asXMLNode(tmp[["thetase"]]), what = "theta")
#' names(tmp)
#' omega = lapply(asXMLNode(tmp[["omega"]])$children, xml.Extract.NM.Matrix)
#'
#' make.symmetric(unlist(omega))
xml.Extract.NM.Matrix = function(y){
  theNames = lapply(y$children, xmlAttrs)
  theValues = as.numeric(unlist(lapply(y$children, xmlValue)))
  if(!is.null(theValues)) names(theValues) = theNames
  return(theValues)
}

if(F)
{
  #require(XML)
  ## this example is based on the first lines of nm.extract.xml
  run = "run31"
  path = nmDir
  xml.extension = ".xml"
  zip.extension = ".7z"
  filename = paste(path, run, paste(run, xml.extension,sep=""),sep="/")
  fz = file.path(nmDir,"run31",paste("run31.xml", ".7z", sep = ""))
  ## unzip the xml file in case it is required
  if(file.exists(fz)&file.exists(filename)) file.remove(filename)
  if(file.exists(fz)) 
    nm.unzip(run = run, path = file.path(path,run), zip.extension = zip.extension, extension = xml.extension)
  
  top = xmlRoot(xmlTreeParse(filename))# names(top)
  TMP = top[["nonmem"]][["problem"]]
  TMP = TMP[grepl("estimation", names(TMP))]
  len = length(TMP)
  ## get estimation titles
  est.methods = sapply(1:length(TMP), function(x,TMP)
    xmlValue(TMP[x][["estimation"]][["estimation_title"]]), TMP = TMP)
  names(TMP) = est.methods
  tmp = TMP[[1]]
  
  names(tmp)
  omega = lapply(asXMLNode(tmp[["omega"]])$children, xml.Extract.NM.Matrix)
  
  make.symmetric(unlist(omega))
#   [1,] 0.06322009 0.00000000    0    0 0.0000000 0.0000000 0.0000000
#   [2,] 0.00000000 0.09022483    0    0 0.0000000 0.0000000 0.0000000
#   [3,] 0.00000000 0.00000000    0    0 0.0000000 0.0000000 0.0000000
#   [4,] 0.00000000 0.00000000    0    0 0.0000000 0.0000000 0.0000000
#   [5,] 0.00000000 0.00000000    0    0 0.1446476 0.0000000 0.0000000
#   [6,] 0.00000000 0.00000000    0    0 0.0000000 0.2231181 0.0000000
#   [7,] 0.00000000 0.00000000    0    0 0.0000000 0.0000000 0.2240212
}
