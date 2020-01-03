#' Format a tabulated summary for LaTeX
#' @param demoTable output from \code{tabSummarize}
#' @param formula the same formula as used to create the \code{tabSummarize} output
#' @return Table with summarized data formatted for inclusion in LaTeX 
#' @seealso \code{\link{tabStats}}, \code{\link{tabSummarize}}
#' @note This function is primarily used for demographics tables
#' @export format.demoTable
#' @import nlme
#' @examples 
#' options(width = 150)
#' pkpdData = example.pkpdData()
#' ok = duplicated(pkpdData$id) == FALSE
#' myFormula =  dose ~ race + wt + bmi + sex
#' my.formatted.demoTable = format.demoTable(
#'  tabSummarize(formula = myFormula, data = pkpdData[ok, ], nSignif = 3)
#'   , formula = myFormula
#' )
#' print(my.formatted.demoTable
#'   , sanitize.text.function = identity
#'   , booktabs = TRUE
#' )


format.demoTable = function(demoTable, formula)
{
  names(demoTable)[1] = "Parameter"
  theParameters = full.names(all.vars(nlme::getCovariateFormula(formula)[[2]]))
  msel = which(demoTable$Parameter %in% theParameters)
  msel2 = which(demoTable$Parameter %nin% theParameters)
  demoTable$Parameter[msel] = paste("\\textbf{", demoTable$Parameter[msel], "}", sep = "")
  demoTable$Parameter[msel2] = paste("\\hfill ", demoTable$Parameter[msel2], sep = "")
  demoTable = reorder.names(demoTable, Cs(Parameter))
  demoTable = apply(demoTable,2, function(x) gsub("\\%","\\\\%",x))
  dimnames(demoTable)[[2]][1] = "~"
  return(demoTable)
}

