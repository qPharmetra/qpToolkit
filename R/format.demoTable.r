#' Format a tabulated summary for LaTeX
#' @param x output from \code{\link{tabSummarize}} of class \code{demoTable}
#' @param \dots ignored
#' @param formula the same formula as used to create the \code{tabSummarize} output
#' @return Table with summarized data formatted for inclusion in LaTeX 
#' @seealso \code{\link{tabStats}}, \code{\link{tabSummarize}}
#' @note This function is primarily used for demographics tables
#' @export
#' @importFrom nlme getCovariateFormula
#' @importFrom Hmisc %nin% label<-
#' @examples 
#' options(width = 150)
#' pkpdData = example.pkpdData()
#' ok = duplicated(pkpdData$id) == FALSE
#' myFormula =  dose ~ race + wt + bmi + sex
#' my.formatted.demoTable = format(
#'  tabSummarize(formula = myFormula, data = pkpdData[ok, ], nSignif = 3)
#'   , formula = myFormula
#' )
#' print(my.formatted.demoTable
#'   , sanitize.text.function = identity
#'   , booktabs = TRUE
#' )

format.demoTable = function(x, ..., formula)
{
  names(x)[1] = "Parameter"
  theParameters = full.names(all.vars(nlme::getCovariateFormula(formula)[[2]]))
  msel = which(x$Parameter %in% theParameters)
  msel2 = which(x$Parameter %nin% theParameters)
  x$Parameter[msel] = paste("\\textbf{", x$Parameter[msel], "}", sep = "")
  x$Parameter[msel2] = paste("\\hfill ", x$Parameter[msel2], sep = "")
  x = reorder(x, 'Parameter')
  x = apply(x,2, function(x) gsub("\\%","\\\\%",x))
  dimnames(x)[[2]][1] = "~"
  return(x)
}

