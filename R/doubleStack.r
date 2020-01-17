#' Stack a Value Twice by Two Covariates
#' @description Dataset will be melted by specified columsn after it will be melted by a second set of columns. This comes in handy when plotting multiple individual parameter estimates versus multiple covariates.
#' @param data dataset containing all variables specified in vars1 and vars2
#' @param vars1 The first variable to stack by. For example \code{c(WT,AGE,BMI,SEX)}
#' @param vars2 The second variable to stack by after the first stack. For example \code{c(ETA1,ETA2,ETA3)}
#' @importFrom reshape2 melt
#' @return A data.frame
#' @export
#'
#' @examples
#' set.seed(1234)
#'
#' myDF = data.frame(id=1:100)
#' myDF$wt = with(myDF, signif(rnorm.by.id(id, 76,15)))
#' myDF$age = with(myDF, signif(sample.by.id(id, samples = seq(18,99))))
#' myDF$sex = with(myDF, sample.by.id(id, samples = c("F","M")))
#' myDF$CL = with(myDF, signif(rnorm.by.id(id, 10,1)))
#' myDF$V = with(myDF, signif(rnorm.by.id(id, 3,0.25)))

#' stacked.df = doubleStack(myDF, vars1 = c("CL","V"), vars2 = c("wt","age","sex"))
#' head(myDF)
#' head(stacked.df)

#' library(ggplot2)
#' ggplot(subset(stacked.df, variable != "sex"), aes(x=as.numeric(value), y = value1)) +
#'    geom_point() +
#'    stat_smooth(method="lm", col = lin.fit.col) +
#'    facet_grid(variable1 ~ variable,scales="free_y")
doubleStack <- function(data, vars1, vars2)
{
  stackedData = reshape2::melt(data,measure.vars=vars1)
  names(stackedData)[names(stackedData) %in% c("value","variable")] = c("variable1","value1")
  stackedData = reshape2::melt(stackedData, measure.vars=vars2)
  stackedData
}

