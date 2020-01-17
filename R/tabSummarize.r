# ROXYGEN Documentation
#' Tabulate a summary
#' @param formula formula specifying what to summarize by what
#' @param data data to summarize
#' @param digits number of significant digits in output
#' @param extra.blank.line a logical indicating if an empty line needs to be inserted between two summaries to improve layout and legibility
#' @param digits.categorical passed to \code{\link{tabStats}}
#' @return Table with summarized data, of class \code{demoTable}
#' @seealso \code{\link{conDataFun1}},  \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{catDataFun}},  \code{\link{tabStats}}
#' @note This function is primarily used for demographics tables
#' @export
#' @importFrom nlme nlme
#' @importFrom nlme getResponseFormula
#' @importFrom nlme getCovariateFormula
#' @examples
#' options(width = 150)
#' pkpdData = example.pkpdData()
#' ok = duplicated(pkpdData$id) == FALSE
#' tabSummarize(formula = dose ~ race + wt + bmi + sex, data = pkpdData[ok, ], digits = 3)
#' pkpdData$sex <- factor(pkpdData$sex, levels = c('M','F'))
#' tabSummarize(formula = dose ~ race + wt + bmi + sex, data = pkpdData[ok, ], digits = 3)
#' # check the categorical summary for race
#' round(table(pkpdData$race[ok], pkpdData$dose[ok]) / apply(table(pkpdData$race[ok],
#' pkpdData$dose[ok]), 2, sum) * 100)
#' # OK
#' myFormula =  dose ~ race + wt + bmi + sex
#' format(
#'  tabSummarize(formula = myFormula, data = pkpdData[ok, ], digits = 3)
#'   , formula = myFormula
#' )
tabSummarize <- function(  formula
                        , data
                        , digits = 3
                        , extra.blank.line = TRUE
                        , digits.categorical = 1)
{
  allX = all.vars(nlme::getResponseFormula(formula)[[2]])
  allY = all.vars(nlme::getCovariateFormula(formula)[[2]])
  BY = lapply(1:length(allX)
              , function(x, allX, data)
                eval(as.name(allX[[x]]), data)
              , data = data
              , allX = allX
  )
  names(BY) = allX

  YYY = lapply(allY, function(yyy, data) eval(as.name(yyy), data), data = data)
  names(YYY) = allY
  theData = do.call("rbind"
                    , lapply(1:length(YYY),
                            function(z
                                     , YYY
                                     , BY
                                     , extra.blank.line
                                     , digits
                                     , digits.categorical)
                            {
                              stats = tabStats(x = YYY[[z]]
                                               , BY = BY
                                               , parName = names(YYY)[z]
                                               , digits = digits
                                               , digits.categorical = digits.categorical
                              )
                              if(extra.blank.line == TRUE)
                              {
                                EBL = stats[1,]
                                EBL[1,] = rep("", ncol(stats))
                                stats = rbind(EBL, stats)
                              }
                              return(stats)
                            }
                            , YYY = YYY
                            , BY = BY
                            , extra.blank.line = extra.blank.line
                            , digits = digits
                            , digits.categorical = digits.categorical
                    )
  )
  row.names(theData) = 1 : nrow(theData)

  theData$parameter = full.names(theData$parameter)
  names.order = as.character(unique(eval(as.name(allX[1]), data))) ## sorting properly
  theData = theData[, c("parameter", names.order, "All")]

  ## insert (N = xxx)
  ndf = theData[1,]
  NNN = tapply(YYY[[1]], BY, length)
  NNN = NNN[names.order]
  ndf[1,] = c("", paste("(N = ",c(as.numeric(NNN), length(YYY[[1]])), ")", sep = ""))#, length(YYY[[1]]))
  theData = rbind(ndf, theData)
  class(theData) <- c('demoTable', class(theData))
  return(theData)
}
