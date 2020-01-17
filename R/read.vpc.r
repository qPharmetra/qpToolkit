
# ROXYGEN Documentation
#' Create covariate plot and data
#' @description An alternative VPC plotting routine.
#' @param path directory where VPC output resides
#' @param result filename of the 'vpc_results.csv" file
#' @param tab filename of the 'vpctab' file
#' @seealso \code{\link{nm.read.vpc}}, \code{\link{ggvpc_standard}}, \code{\link{ggvpc_xpose}}
#' @return list of data frames used to plot a VPC
#' @importFrom xpose4 read.npc.vpc.results
#' @importFrom utils read.table
#' @export
#' @note This function is used by nm.read.vpc and is intended for that purpose only; running this function alone will not help the average R programmer. Please note that qPharmetra's default VPC plot tool is xpose.VPC. Use this tool in case one needs more flexibility to post-process results before plotting, and when stratifcation by more than one variable is needed.
#' @importFrom xpose4 read.npc.vpc.results


read.vpc <- function(path = "./", result = result, tab = tab)
{
   #if(length(grep(":", path)) == 0) path = paste(getwd(), path, sep = "/")
   vpc.results = read.npc.vpc.results(vpc.results =  file.path(path, result))
   vpc.tab = read.table(paste(path, tab, sep = "/"), header = TRUE, sep = ",")
   if(length(vpc.tab$"strata_no") == 0)
   {
      vpc.results$result.tables = list(vpc.results$result.tables)
      vpc.results$result.tables = c(vpc.results$result.tables, "unstratified")
      vpc.results$strata.names = "1"
      vpc.tab$"strata_no" = rep(1, nrow(vpc.tab))
   }
   list(vpc = vpc.results, tab = vpc.tab)
}
