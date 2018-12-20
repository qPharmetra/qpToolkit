# name:     get.dOFV
# purpose:  create a brief run record based on PsN's runrecord tags in NONMEM control/ output files
# input:    runs and the path the rundirectories resides 
# output:   data.frame
# note: 

# ROXYGEN Documentation
#' Create brief NONMEM run record
#' @description rather than calling PsN's runrecord (the full blown parser) get.dOFV create a brief runrecord on the fly. It might be used for interim reports and presentations but ideally formal reporting uses
#' @param runs character vector of existing NONMEM runs
#' @param path directory where NONMEM output resides 
#' @param conf.level confidence level - currently not used
#' @param file.ext file extension of the NONMEM output file (defaults to .lst)
#' @return Placeholder for return description
#' @export
#' @import Hmisc
#' @seealso \code{\link{get.ofv}} which is called inside \code{get.dOFV}, \code{\link{read.out}}
#' @examples
#' get.dOFV(runs = c("example1","example2"), path = getOption("qpExampleDir"))
#' # note this example is for two non-nested models. It has not other value to to illustrate the tool

get.dOFV =  function(runs = Cs(run1,run2)
                     , path = getOption("nmDir")
                     , conf.level = 0.95
                     , file.ext = ".lst"
)
{
   if(length(runs)<2 || any(sapply(runs, class)!="character"))
   {
      message("argument 'runs' needs to be a character vector of at least length 2")
   }
   
   ## check which runs are there
   ok.runs = sapply(runs, function(run) file.exists( paste(path, paste(run, file.ext, sep=""), sep="/")))
   if(sum(ok.runs)<=1) {message("only one parseable run.lst found. No model building table created.");return()}
   runs = runs[ok.runs]
   
   ## get the based on number
   lst = sapply(runs, function(x, path, file.ext) read.out(x, path = file.path(path), file.ext = file.ext)
                , path=path, file.ext = file.ext)
   base = lapply(lst, function(x) x[grep("Based on:", x)])
   base = lapply(base, function(x) extract.number(sub(".*Based on: ([A-Z,a-z,0-9]+)","\\1",x)[1]))
   
   ## get description statement
   Description = lapply(lst, function(x) substring(x[grep("Description:", x)+1],7))
   Description = lapply(Description, function(x) ifelse(length(x)==0,"No Description",x))
   
   ## work on OFV and delta OFV 
   result = sapply(runs,function(x, path, file.ext) {
      ofv = get.ofv(x, path = path, file.ext = file.ext)
      ofv[length(ofv)] ## use the last one
   }
   , path = path, file.ext = file.ext)
   if(is.list(result)) result = unlist(lapply(result, function(x) x[length(x)]))
   dOFV = data.frame(OFV = result, Run = extract.number(runs), Ref = unlist(base))
   dOFV$dOFV = c(NA, dOFV$OFV[-1]-dOFV$OFV[match(dOFV$Ref, dOFV$Run)][-1])
   
   ## put things together
   result = data.frame(Run = extract.number(runs), Ref = unlist(base), OFV = result, dOFV = dOFV$dOFV, Description = unlist(Description))
   row.names(result) = seq(nrow(result))
   result
}


