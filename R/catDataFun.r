
# ROXYGEN Documentation
#' Summary: length (\%) 
#' @param y a vector of values to be summarised.
#' @param ndigits.categorical number of digits to round to
#' @return A formatted character vector containing count and percentage of 
#' unique values in \code{y}.
#' @seealso \code{\link{conDataFun1}},  \code{\link{conDataFun2}},  \code{\link{conDataFun3}},  \code{\link{tabStats}},  \code{\link{tabStats}}
#' @export
#' @import Hmisc
#' @examples
#' catDataFun(data.frame(Sex=rep(c("Female","Male"), times= c(40,60)))$Sex)

catDataFun = function (y, ndigits.categorical = 1) 
{
  sprint.text = paste0("%#.",ndigits.categorical,"f") 
  myCatSummary = round(100 * table(y)/sum(table(y)),digits=ndigits.categorical)
  myCat = if(ndigits.categorical>0) {
     myCat = paste0(table(y), " (",sprintf(sprint.text, myCatSummary), "%)")
     } else paste0(table(y), " (", myCatSummary, "%)")
  return(myCat)
}

