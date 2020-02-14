#' Formalize Something
#' 
#' Formalizes something.
#' Generic, with method \code{\link{formalize.data.frame}}.
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @family formalize
#' @return see methods
#' @keywords internal
formalize <- function(x, ...)UseMethod('formalize')

#' Formalize Data Frame
#' 
#' Formalizes data.frame.
#' Replaces column names with labels, where present.
#' Stores column name as 'alias' attribute.
#' 
#' @param x data.frame
#' @param ... passed arguments
#' @export
#' @family formalize
#' @return formalized data.frame
#' @examples
#' x <- data.frame(x = 1:10, y = 1:10, z = 1:10)
#' attr(x$x, 'label') <- 'Independent Value'
#' attr(x$y, 'label') <- 'Dependent Value'
#' x
#' formalize(x)
formalize.data.frame <- function(x, ...){
   for(col in names(x)){
      lab <- attr(x[[col]], 'label')
      attr(x[[col]], 'alias') <- 'col'
      if(length(lab) == 1) names(x)[match(col, names(x))] <- lab
   }
   class(x) <- union('formalized', class(x))
   x
}
