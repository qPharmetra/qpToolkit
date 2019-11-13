# name:     bootstrap.ParTab
# purpose:  Create parameter table from NONMEM bootsrap
# input:    path of bootstrap file location, the filenames for the raw_results and the results.csv files
# output:   list of bootstrap results
# note:  

# ROXYGEN Documentation
#' Create parameter table from NONMEM bootsrap
#' @param bootstrap A bootstrap object (list) created by read.bootstrap.
#' @param idx list of which parameters to include.
#' @param probs The confidence interval coverage.
#' @param central function for central tendency of distribution
#' @param nsig number of significant digits in output
#' @return A data.frame with the bootstrap estimates.
#' @export bootstrap.ParTab
#' @examples
#' myBoot = read.bootstrap(
#'  path = getOption("qpExampleDir"),
#'  filename = "bootstrap/raw_results_bs4011.csv",
#'  structure.filename = "bootstrap/raw_results_structure"
#' )
#' bootstrap.ParTab(myBoot, idx = list(theta=1:13,omega=1:7,sigma=1))
bootstrap.ParTab = function(bootstrap
                            , idx = list(theta=1,omega=1,sigma=1)
                            , probs = 0.95
                            , central = median
                            , nsig = 3)
{
  #require(metrumrg)
  ci = c((1-probs)/2,(probs+1)/2)
  out = data.frame(t(apply(bootstrap$bootstrap[-1,], 2, quantile, probs=ci)))
  names(out) = c('Lower', 'Upper')
  out$Estimate = signif(apply(bootstrap$bootstrap[-1,], 2, central), nsig)
  out$Orig.Estimate = signif(unlist(bootstrap$bootstrap[1,]), nsig)
  out = out[7:nrow(out), ]
  thn = paste("THETA", idx$theta,sep="")
  omn = if(!is.null(idx$omega)) paste("OMEGA", idx$omega,sep="") else character(0)
  sgn = if(!is.null(idx$sigma)) paste("SIGMA", idx$sigma,sep="") else character(0)
  parries = c(thn,omn,sgn)
  if(length(parries) != nrow(out)) parries = paste("THETA", 1:nrow(out),sep="")
  out$Parameter = parries
  out$Descriptor = row.names(out)
  out$CI95 = paste(signif(out$Lower,3), "-", signif(out$Upper,3))
  out = out[, c("Parameter","Descriptor","Orig.Estimate","Estimate","CI95")]
  row.names(out) = 1:nrow(out)
  return(out)
}

