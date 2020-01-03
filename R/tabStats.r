# name:     tabStats
# purpose:  determines is variable is continuous or categorical and returns the statistics
# input:    numeric or character vector, summarization functions
# output:   character vector
# note:     is used for demographics tables

## tabStats 

# ROXYGEN Documentation
#' Table Statistics
#' @description Determines if variable is continuous or categorical and returns the appropriate summary statistics.
#' @param x numeric vector to summarize
#' @param BY stratification argument
#' @param nSignif number of significant digits in continuous data summaries
#' @param conFunc1  defaults to conDataFun1
#' @param conFunc2  defaults to conDataFun2
#' @param catFunc  defaults to catDataFun
#' @param parName is automatically populated but the parameter name can be specified here by the user.
#' @return Appropriate statistic for x
#' @note This function is primarily used for demographics tables
#' @seealso \code{\link{conDataFun1}},  \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{catDataFun}},  \code{\link{tabSummarize}} 
#' @export
#' @examples  
#' pkpdData = example.pkpdData()
#' ok = duplicated(pkpdData$id) == FALSE
#' tabStats(x=pkpdData$race[ok], BY=list(dose = pkpdData$dose[ok]))
#' tabStats(x=pkpdData$race[ok], BY=list(dose = pkpdData$dose[ok]), parName = "Race", ndigits.categorical=0)
#' tabStats(pkpdData$sex[ok], list(dose = pkpdData$dose[ok]))
#' tabStats(x=pkpdData$wt, BY = list(dose = pkpdData$dose))
#' tabStats(x=pkpdData$bmi, BY = list(dose = pkpdData$dose))

tabStats = function (x, BY, nSignif = 3, conFunc1 = conDataFun1, conFunc2 = conDataFun2, 
                     catFunc = catDataFun, ndigits.categorical = 1, parName) 
{
  if (missing(parName)) {
    parName = deparse((match.call()[2]))
    parName = substring(parName, 1, (nchar(parName) - 2))
  }
  BY = lapply(BY, function(by) c(as.character(by), rep("All", 
                                                       length(by))))
  if (is.factor(x) | is.character(x)) {
    x = unlist(list(x, x))
    if (is.character(x)) 
      x = as.factor(x)
    tmp = data.frame(t(aggregate(x, by = BY, FUN = catFunc, 
                                 ndigits.categorical = ndigits.categorical)))
    row.names(tmp)[(length(names(BY)) + 1):nrow(tmp)] = levels(x)
  }
  if (is.numeric(x)) {
    x = c(x, x)
    tmp1 = data.frame(t(aggregate(x, by = BY, FUN = function(y, nSignif) 
      conFunc1(y, nSignif = nSignif), nSignif = nSignif)))
    tmp2 = data.frame(t(aggregate(x, by = BY, FUN = function(y, nSignif) 
      conFunc2(y, nSignif = nSignif), nSignif = nSignif)))
    tmp = rbind(tmp1, tmp2[2, ])
  }
  names(tmp) = levels(as.factor(unlist(BY)))
  tmp2 = as.data.frame(tmp[, 1])
  names(tmp2) = "parameter"
  tmp2$parameter = row.names(tmp)
  tmp2$parameter[1] = parName
  if (is.numeric(x)) 
    tmp2$parameter = c(parName, "Mean (SD)", "Median (range)")
  tmp = cbind(tmp2, tmp)
  row.names(tmp) = 1:nrow(tmp)
  for (i in 1:ncol(tmp)) tmp[, i] = as.character(tmp[, i])
  tmp[1, ] = c(parName, rep("", length(2:ncol(tmp))))
  return(tmp)
}
