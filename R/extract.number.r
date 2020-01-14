# name:     extract.number
# purpose:  takes character string and parses the numeric input from it, if available.
# input:    character or numeric string
# output:   numeric
# note:     if multiple blocks of numeric input exist wthin the input, only the first is parsed

# ROXYGEN Documentation
#' Takes character string and parses the numeric input from it, if available.
#' @section Notes:
#' If multiple blocks of numeric input exist wthin the input, only the first is parsed
#' @param x A character or numeric string.
#' @return A numeric vector if the input has multiple character blocks.
#' @export
#' @seealso \code{\link{extract.character}}
#' @examples
#' extract.number("c76")
#' extract.number("c76c7")
#' extract.number("BLOQ = 0.144")

extract.number = function(x)
{
  x = as.character(x)
  w1 = regexpr("[+-]?[0-9.]+", x)
  as.numeric(substring(x, w1, w1 + attr(w1, "match.length")-1))
}


