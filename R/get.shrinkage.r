
# ROXYGEN Documentation
#' NONMEM shrinkage estimates
#' @description Parse shrinkage values from NONMEM output file
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext extension of the NONMEM output file (.ext)
#' @return shrinkage values
#' @export get.shrinkage
#' @seealso \code{\link{get.nm.version}}, \code{\link{get.ofv}}, \code{\link{read.out}}
#' @import Hmisc lattice
#' @examples
#' get.shrinkage(run = "example1", path = getOption("qpExampleDir"))

get.shrinkage = function(run, path = getOption(nmDir), file.ext = ".lst")
  
{ 
  out = read.out(path=path, run=run, file.ext = file.ext)
  version = get.nm.version(path=path, run=run, file.ext = file.ext)
  
  txtETAStart = grep("ETAshrink", out)
  txtEBVStart = grep("EBVshrink", out)
  txtEPSStart = grep("EPSshrink", out)
  
  ## parsing text
  txtETAParse = as.list(seq(length(txtETAStart)))
  txtEPSParse = txtEBVParse = txtETAParse
  for(i in 1:length(txtETAStart))
  {
    txtETAParse[[i]] = txtETAStart[i]  : (txtEBVStart[i]-1)
    txtEBVParse[[i]] = txtEBVStart[i]  : (txtEPSStart[i]-1)
    txtEPSParse[[i]] = txtEPSStart[i] 
  }
  etaShrink = lapply(txtETAParse, function(x, out) 
    trimSpace(substring(paste(out[x], collapse = ""),16)), out = out)
  ebvShrink = lapply(txtEBVParse, function(x, out) 
     trimSpace(substring(paste(out[x], collapse = ""),16)), out = out)
  epsShrink = lapply(txtEPSParse, function(x, out) 
     trimSpace(substring(paste(out[x], collapse = ""),16)), out = out)
  
  ## text to parse for the estimation method
  nameParse = as.list(seq(length(etaShrink)))
  txtOBJParse = grep("FINAL PARAMETER ESTIMATE",out)
  for(i in 1:length(nameParse))
  {
    nameParse[[i]] = trimSpace(sub("[*]+","",sub("[*]+","",out[txtOBJParse[i]-1])))
  }
  names(etaShrink) = nameParse
  names(ebvShrink) = nameParse
  names(epsShrink) = nameParse

  return(list(version=version, eta = etaShrink, ebv = ebvShrink, eps = epsShrink))
  
}


