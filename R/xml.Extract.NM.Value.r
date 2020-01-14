# name:     xml.Extract.NM.Value
# purpose:  extracts numeric vector from a NONMEM xml vector node
# input:    NONMEM xml vector node
# output:   vector (numeric)
# note:     is called by qP function nm.extract.xml

# ROXYGEN Documentation
#' XML parser for vectors
#' @description Extract XML content into a numeric vector
#' @param x The 'theta' element from NONMEM output in XML format
#' @param what character string, like 'theta' or 'thetase'
#' @return Numeric vector
#' @seealso \code{\link{nm.extract.xml}}
#' @import XML
#' @export xml.Extract.NM.Value
#' @examples
#' \dontrun{
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
#' }

xml.Extract.NM.Value = function(x, what = "theta")
{
  theValues = unlist(lapply(x$children, function(y) as.numeric(xmlValue(y))))
  theNames = paste(casefold(what,upper=TRUE),
                   unlist(lapply(x$children, xmlAttrs)), sep="")
  if(!is.null(theValues)) names(theValues) = theNames
  return(theValues)
}
