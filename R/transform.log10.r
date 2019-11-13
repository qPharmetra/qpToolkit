# name:     transform_log10
# purpose:  Takes the log10 of the input in case argument log is T or a numeric value
# input:    numeric vector
# output:   numeric vector
# note:     is used in plot.fit

# ROXYGEN Documentation
#' Log10 transformation
#' @description Takes the log10 of the input in case argument log is T or a numeric value
#' @param x Value to log with base 10
#' @param log If TRUE or any numeric value the log base 10 of x will be taken
#' @return log10(x)
#' @export
#' @examples
#' transform_log10(c(1,10,100), log = TRUE)
#' transform_log10(c(2,3,200,1000,33330), log = 2)
transform_log10 = function(`_data`, ..., log)
{
  if({is.logical(log) & log == TRUE} || is.numeric(log)) log10(`_data`) else `_data`
}

