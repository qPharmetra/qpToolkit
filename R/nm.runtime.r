
# ROXYGEN Documentation
#' NONMEM run time
#' @description Get the NONMEM run time
#' @param run run rootname (e.g. \code{'run1'})
#' @param path directory where \code{run} resides
#' @seealso \code{\link{get.nm.version}}, \code{\link{get.shrinkage}}, \code{\link{get.ofv}}, \code{\link{read.out}}
#' @examples
#' nm.runtime(run = "example1", path = getOption("qpExampleDir"))
#' @export nm.runtime
nm.runtime = function(run, path = getOption("nmDir"))
{
   out = read.out(run=run, path = path)
   meth = gsub(" #METH: ","",out[grep("#METH",out)])
   est = extract.number(out[grep("estimation time",out)])
   covt =  extract.number(out[grep("covariance time",out)])
   
   runtime = shuffle.list(list(paste0("Estimation: ", round(est/3600,2),"h")
     , paste0("Covariance Step: ",round(covt/3600,2),"h")
     , paste0("Total: ",round((est+covt)/3600,2),"h")
      )
   )
   runtime = lapply(runtime, unlist)
   names(runtime) = meth
   runtime
}