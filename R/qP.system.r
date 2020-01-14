#' Print a welcome message
#'
#' @param name length one character: who is being addressed
#' @return A welcoming message
#' @keywords internal
#' @importFrom knitr knit
# importing from knitr to suppress warning: need vignette builder but no other imports

qP.welcome = function(name="") cat(sep = '', "Welcome to the qPharmetra Toolkit for R", if(name == '') '\n' else paste0(', ', name, '\n'))
