# ROXYGEN Documentation
#' Forced length significant digits
#' @param x a vector of values to be processed into significant digits format
#' @param digits single value specifying the number of significant digits
#' @return A formatted character vector containing \code{x} in with significant \code{digits} format
#' @seealso \code{\link{signif}}
#' @export
#' @examples
#' formatted.signif(c(5,4.99, 4.99999,5.0000001,5001),4)
#' 
#' numvec = c(5,4.99, 4.99999,5.0000001,5001, 0.00005,101,39.9)
#' print(
#'    xtable(
#'       data.frame(original=numvec
#'                  ,latex.TRUE= formatted.signif(numvec,3, T,F)
#'                  ,align.TRUE= formatted.signif(numvec,3, F,T)
#'                  ,all.TRUE = formatted.signif(numvec,3, T,T)
#'       )
#'    )
#'    , booktabs=T
#'    , sanitize.text.function = identity
#' )
#' # now take this to a LaTeX compiler!


formatted.signif = function(x, digits=3, latex = F, align.dot = F){
  res = vector(length=length(x))
  for(i in 1:length(x))
  {
    res[i] = sprintf(paste0("%#.",digits,"g"), signif(x[i],digits))
    if(substring(res[i],nchar(res[i])) == ".") res[i] = substring(res[i],1,(nchar(res[i])-1)) ## remove trailing dot
  }
  if(latex) for(i in 1:length(x))
  {
    if(grepl("e", res[i])){
      tmp = unPaste(res[i], sep = "e")
      res[i] = paste0(tmp[[1]],"$\\cdot$10$^{", as.numeric(tmp[[2]]),"}$")
    }
  }
  if(align.dot){
    res = align.around.decimal.point(res)
  }
  return(res)
}

