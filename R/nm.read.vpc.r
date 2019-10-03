# name:     nm.read.vpc
# purpose:  parses the output from PsN's vpc and prepares it for plotting 
# input:    path and filenames of the vpc output
# output:   list of data frames 'obs', 'vpc, and 'res'
# note:     

# ROXYGEN Documentation
#' Read VPC output
#' @description Parses the output from PsN's vpc and prepares it for plotting.
#' @param path directory where vpc_results.csv and vpctab files reside
#' @param vpc_results file name of summarized VPC results. Defaults to vpc_results.csv
#' @param vpctab filename of observed data table. Defaults to vpctab
#' @param PI.limits what prediction interval stats should be read from the VPC output files
#' @param PI.ci.area.smooth if TRUE the center of the interval of the independent variable will be used. This will create smooth VPC plots, the same way as xpose does. (defaults to FALSE)
#' @return A list of data frames 'obs', 'vpc, and 'res'
#' @note Please note that qPharmetra's default VPC plot tool is xpose.VPC. Use this function with the \code{ggvpc} functions in case one needs more flexibility to post-process results before plotting, and when stratifcation by more than one variable is needed.
#' @export
#' @seealso \code{\link{ggvpc_xpose}},  \code{\link{ggvpc_standard}}, \code{\link{read.vpc}}
#' @import xpose4
#' @importFrom Hmisc unPaste Cs
#' @examples
#' myVPC = nm.read.vpc(path = file.path(getOption("qpExampleDir"), "vpc_final_strt"))
#' 
#' unique(myVPC$vpc$strata)
#' unique(myVPC$obs$strata)
#' # they match nicely

nm.read.vpc = function(path = getOption("nmDir") 
                       , vpc_results = "vpc_results.csv"
                       , vpctab = dir(path=path,pattern = "^vpctab")[1]
                       , PI.limits = c(0.025,0.05,0.1,0.30,0.70,0.9,0.95,0.975)
                       , PI.ci.area.smooth=FALSE
)
{
  rootName = unlist(unPaste(path)[(length(unPaste(path)))])
  
  ## parse out the vpc results into predicted (sim) and observed (obs)
  vpc = read.vpc(path = path, result = vpc_results, tab=vpctab)
  
  sim = vpc$vpc
  obs = vpc$tab
  
  names(obs)
  
  responseVar = vpc$vpc$dv.var
  xCov = vpc$vpc$idv.var
  
  ## pull out the correct statistics
  msel.ci = grep(".CI.for.", names(sim$result.tables[[1]]))
  suffix =  unique(sub("([0-9]+).CI.for(.*)","\\1",names(sim$result.tables[[1]])[msel.ci]))               
  msel.ci = grep(paste(suffix,".CI.for.",sep=""), names(sim$result.tables[[1]]))
  msel.pi = sub("([0-9]+).CI.for(.*)","\\2",names(sim$result.tables[[1]]))
  msel.pi = sub("to", "", msel.pi)
  msel.pi = sub("from", "", msel.pi)
  msel.pi = substring(msel.pi, 2, (nchar(msel.pi)-1))
  msel.pi = which(msel.pi %in% as.character(c(PI.limits*100,50)))
  ci.names = names(sim$result.tables[[1]])[intersect(msel.ci,msel.pi)]
  
  real.names = names(sim$result.tables[[1]])[grep("real", names(sim$result.tables[[1]]))]
  real.names2 = sub("([0-9]+).real","\\1",real.names)
  real.names2 = real.names[real.names2 %in% as.character(c(PI.limits*100,50))]
  real.names = names(sim$result.tables[[1]])[names(sim$result.tables[[1]]) %in% real.names2]
  
  sim.names = names(sim$result.tables[[1]])[grep("sim", names(sim$result.tables[[1]]))]
  sim.names2 = sub("([0-9]+).sim","\\1",sim.names)
  sim.names2 = sim.names[sim.names2 %in% as.character(c(PI.limits*100,50))]
  pred.names = names(sim$result.tables[[1]])[names(sim$result.tables[[1]]) %in% sim.names2]
  
  ## organize predicted results
  names(sim$result.tables[[1]])
  sim$result.tables = sim$result.tables[unlist(lapply(sim$result.tables, function(x) 
    class(x)  == "data.frame"))]
  sim$result.tables = lapply(seq(along = sim$result.tables), function(x, res, nams){
    out = data.frame(res[[x]]); out$strata = nams[x]; return(out)
  }, nams = sim$strata.names, res = sim$result.tables)
  results.tables = do.call("rbind",sim$result.tables)
  names(results.tables) = sub("X", "", names(results.tables))
  results.tables = results.tables[, c("lower","upper",pred.names, real.names, ci.names, "strata")]
  results.tables$xCovm = rowMeans(results.tables[, Cs(lower,upper)])
  names(results.tables)[grep("[.]", names(results.tables))] = 
    paste0("vpc", names(results.tables)[grep("[.]", names(results.tables))])
  
  #vpc$vpc$strata.names;   obs$strata_no
  obs$strata = swap(obs$strata_no, sunique(obs$strata_no), vpc$vpc$strata.names)
  obs$dv.var = vpc$vpc$dv.var
  obs$idv.var = vpc$vpc$idv.var
  
  ## reshape the vpc element for plotting
  res = results.tables[rep(seq(nrow(results.tables)), ea = 2),]
  res$idx = 1:2
  res$xCov = res$lower * (res$idx == 1) + res$upper * (res$idx == 2)
  if(PI.ci.area.smooth) res$xCov = res$xCovm
  
  return(list(obs = obs, vpc = results.tables, res= res))
}



