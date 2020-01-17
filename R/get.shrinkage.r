
# ROXYGEN Documentation
#' NONMEM shrinkage estimates
#' @description Parse shrinkage values from NONMEM output file
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext extension of the NONMEM output file (.ext)
#' @return shrinkage values
#' @export get.shrinkage
#' @seealso \code{\link{get.nm.version}}, \code{\link{get.ofv}}, \code{\link{read.out}}
#' @examples
#' get.shrinkage(run = "example1", path = getOption("qpExampleDir"))
get.shrinkage <- function(run, path = getOption('nmDir'), file.ext = ".lst")

{
  out = read.out(path = path, run = run, file.ext = file.ext)
  version = get.nm.version(path = path, run = run, file.ext = file.ext)

  # Here we are going to retrograde the list file to look/act like pre 7.4 list file
  # find all the lines of code containing SHRINKVR

  shrinkvr <- grep('SHRINKVR', out)

  # find all the lines of code containing SHRINKSD or TOTAL DATA POINTS

  shrinksd <- grep('SHRINKSD|TOTAL DATA POINTS', out)

  # for each shrinkvr, what is the smallest greater shrinksd?

  smallestGreater <- function(x, pool){
    stopifnot(length(x) == 1)
    candidates <- pool[pool > x]
    min(candidates)
  }

  stop_at <- sapply(shrinkvr, smallestGreater, pool = shrinksd)

  sequences <- lapply(
    seq_along(shrinkvr),
    function(i)seq(
      from = shrinkvr[[i]],
      to = stop_at[[i]] - 1
    )
  )

  bad <- unlist(sequences)
  if(length(bad))out <- out[-1 * bad]

  out <- sub('SHRINKSD','shrink',out)

  ### and carry on as usual

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

  return(list(version = version, eta = etaShrink, ebv = ebvShrink, eps = epsShrink))

}


