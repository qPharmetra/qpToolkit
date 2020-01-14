# name:     get.multiple.xpose.tables
# purpose:  imports output tables of multiple runs and merges them into one dataframe
# input:    runs (vector) and path where NM run folders reside
# output:   data frame with NONMEM table output and colum 'model' to distinguish NONMEM runs
# note:     use argument 'carryAlong' to ask for more data items to be included in the result

# ROXYGEN Documentation
#' Get xpose tables of multiple runs
#' @description Imports output tables of multiple runs and merges them into one data.frame
#' @param runs character vector of run roon names (e.g. run1)
#' @param path directory where run directories of runs reside
#' @param carryAlong character vector of variables to get in addition to ID, PRED, IPRED, CWRES, DV. Argument carryAlong defaults to DOSE TIME and EVID. Use in case output tables are not consistent across runs being imported
#' @note     use argument 'carryAlong' to ask for more data items to be included in the result
#' @return data.fame the xpose tables for runs and an additional column 'model' containinng the rootnames of runs for stratification in post-processing
#' @export
#' @seealso \code{\link{get.xpose.tables}}
#' @examples
#' library(lattice)
#'  test = get.multiple.xpose.tables(runs = c("example1","example2")
#'  , path = getOption("qpExampleDir"), carryAlong = c("CONC","TIME","EVID"))
#' names(test)
#' head(subset(test,model == "example1"))
#' head(subset(test,model == "example2"))
#' xyplot(CWRES ~ value | variable * model
#'        , data = reshape2::melt(test, measure.vars = c('TIME','PRED'))
#'        , panel = panel.cwres
#'        , scales = list(x = list(relation = "free"))
#' )
get.multiple.xpose.tables = function(runs, path = getOption("nmDir"), carryAlong = c("DV","DOSE","TIME","EVID"))
{
  do.call("rbind"
          , lapply(runs, function(i, path){
            data = get.xpose.tables(run = i, path = path)
            data$model = rep(i, nrow(data))
            data[, unique(c('model','ID','PRED','IPRED','CWRES',carryAlong))]
          }, path = path)
  )
}


