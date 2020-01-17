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
#' @param digits number of significant digits in output
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @param transformations a list with elements log or logit, that, in turn, are numeric vectors representating the THETA numbers that are logged or logit-transformed, respectively.
#' @return A data.frame with the bootstrap estimates.
#' @export bootstrap.ParTab
#' @examples
#' myBoot = read.bootstrap(
#'  path = getOption("qpExampleDir"),
#'  filename = "bootstrap/raw_results_bs4011.csv",
#'  structure.filename = "bootstrap/raw_results_structure"
#' )
#' bootstrap.ParTab(myBoot, idx = list(theta = 1:13,omega = 1:7,sigma = 1))
#' bootstrap.ParTab(myBoot, idx = list(theta = 1:13,omega = 1:7,sigma = 1))[c(13,14,15),]
#' bootstrap.ParTab(myBoot, idx = list(theta = 1:13,omega = 1:7,sigma = 1),
#' transformations = list(log = 13:14, logit = 15))[c(13,14,15),] # partly fictional
bootstrap.ParTab <- function(
  bootstrap,
  idx = list(theta = 1,omega = 1,sigma = 1),
  probs = 0.95,
  central = median,
  digits = 3,
  latex = FALSE,
  align.dot = FALSE,
  transformations
){
  myFun <- function(x, type)
  {
    switch(type,
           log = exp(x),
           log10 = 10^x,
           logit = logit.inv(x))
  }

  stopifnot(length(probs) == 1)
  ci = c((1-probs)/2,(probs+1)/2)
  out = data.frame(t(apply(bootstrap$bootstrap[-1,], 2, quantile, probs = ci)))
  names(out) = c('Lower', 'Upper')
  out$Estimate = apply(bootstrap$bootstrap[-1,], 2, central)
  out$Orig.Estimate = unlist(bootstrap$bootstrap[1,])
  out = out[7:nrow(out), ]
  thn = paste("THETA", idx$theta,sep = "")
  omn = if(!is.null(idx$omega)) paste("OMEGA", idx$omega,sep = "") else character(0)
  sgn = if(!is.null(idx$sigma)) paste("SIGMA", idx$sigma,sep = "") else character(0)
  parries = c(thn,omn,sgn)
  if(length(parries) != nrow(out)) parries = paste("THETA", 1:nrow(out),sep = "")
  out$Parameter = parries
  out$Descriptor = row.names(out)
  ## perform transformations if needed
  out$transformed = rep("no",nrow(out))
  if(!missing(transformations))
    if(is.list(transformations))
    {
      len = length(transformations)
      for(x in seq(len))
      {
        ok = transformations[[x]]
        nms = names(transformations)[x]
        out$transformed[ok] = rep(nms, length(ok))

        out$Estimate[ok] = myFun(out$Estimate[ok],nms)
        out$Orig.Estimate[ok] = myFun(out$Orig.Estimate[ok],nms)
        out$Lower[ok] = myFun(out$Lower[ok],nms)
        out$Upper[ok] = myFun(out$Upper[ok],nms)
      }
    }
  out$Estimate = formatted.signif(
    out$Estimate,
    digits = digits,
    latex = latex,
    align.dot = align.dot
  )
  out$Orig.Estimate = formatted.signif(
    out$Orig.Estimate,
    digits = digits,
    latex = latex,
    align.dot = align.dot
  )
  out$CI95 = paste(
    formatted.signif(
      out$Lower,
      digits = digits,
      latex = latex,
      align.dot = align.dot
      ), "-",
    formatted.signif(
      out$Upper,
      digits = digits,
      latex = latex,
      align.dot = align.dot
    )
  )
  out = out[, c("Parameter","Descriptor","Orig.Estimate","Estimate","CI95","transformed")]
  row.names(out) = 1:nrow(out)
  return(out)
}

