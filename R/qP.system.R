#' Print a welcome message
#' 
#' @param name A Character string.  Whom is being addressed (default is "Dude").
#' @return A welcoming message
#' @keywords internal

qP.welcome = function(name="Dude") cat(paste0("Welcome to the qPharmetra Toolkit for R, ", name))

