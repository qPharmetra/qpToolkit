
# ROXYGEN Documentation
#' Trimming spaces
#' @description Remove space from beginning and/or end of a string (trim).
#' @param x character string
#' @return A character string with any spaces at its beginning or end trimmed off
#' @export
#' @examples
#' trimSpace(" Hello ")
#' trimSpace(" Hello there ")

trimSpace = function (x) gsub("^\\s+|\\s+$", "", x)
