# name: tabulate.samples
# purpose: given a dataframe and items that describe it, create a tabular summary of any
#         column that contains samples of a analyte from a drug trial, including a
#         summary of any special values.
# input: a dataframe, the name of one of its columns that itself contains
#         samples of an analyte from a drug trial, and a vector of any odd codings (
#         for example, text that indicates the observation was below the limit of
#         quantification)
# output: a named numeric vector, summarizing the analyte

# ROXYGEN Documentation
#' Summarize endpoints for numeric/non-numeric
#' @description Given a dataframe and items that describe it, create a tabular summary of any column that contains samples of a analyte from a drug trial, including a summary of any special values.
#' @param data dataset with endpoints to summarize
#' @param analyte the endpoint to analyze, e.g. "conc" or "effect"
#' @param oddCode charatcer vector with entries that are not considered numeric like 'BLOQ", "<LOQ", or "Missing Sample"
#' @return Named vector of numeric and oddCode entries
#' @export
#' @seealso \code{\link{whichNumeric}}
#' @examples
#' my.df = data.frame( analyte.1=c(1,2,3,4,'BLOQ',5,6,'NS',7,'M'),
#'                    analyte.2=c(1,2,'BLOQ','M',3,'BLOQ',5,6,'NS',7))
#' tabulate.samples(data=my.df, analyte="analyte.1")
#' tabulate.samples(data=my.df, analyte="analyte.2")

tabulate.samples = function(data, analyte = "conc", oddCode = c('BLOQ','NS','M'))
{
  if(any(is.na(data[, analyte]))) {
    message("column'",analyte,"' contains NA records. Solve this first.")
    return()
  }

  resTab = table(data[, analyte])
  resTab = resTab[names(resTab) %in% oddCode]

  missingItems = oddCode %nin% names(resTab)
  if(any(missingItems)){
    mstuff = rep(0, length(oddCode[missingItems]))
    names(mstuff) = oddCode[missingItems]
    resTab = c(resTab, mstuff)
  }

  resTab = resTab[match(oddCode, names(resTab))]
  resTab = c(resTab, Numeric.Values = length(data[data[, analyte] %nin% oddCode, analyte]))
  myAll = sum(resTab)
  resTab = c(resTab, All = myAll)
  return(resTab)
}


