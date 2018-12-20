
# ROXYGEN Documentation
#' Which entries are numeric?
#' @description Identify which members of a character vector will translate to numeric mode
#' @param x a vector
#' @return position in the character of the elements that have numbers that can be extracted
#' @export
#' @examples
#' test = c("123xyc","A","<LOQ","547.39", "<0.15")
#' whichNumeric(test)
#' extract.number(test[whichNumeric(test)])

whichNumeric = function(x){
  # Identify which members of a character vector will translate to numeric mode
  numsel = grep("[0-9]", x)
  charsel = grep("[:alpha:]", x)
  expsel = grep("[eE]+[+-]+", x)
  gtltsel = grep("[<>=]", x)
  return(numsel[numsel %nin% charsel[!(charsel %in% expsel)] & numsel %nin% gtltsel])
}


