#' Print a welcome message
#' 
#' @param name length one character: who is being addressed
#' @return A welcoming message
#' @keywords internal

qP.welcome = function(name="") cat(sep = ', ', "Welcome to the qPharmetra Toolkit for R", if(name == '') NULL else name)
