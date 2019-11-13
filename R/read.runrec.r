# name:     read.runrec
# purpose:  read the run record into readable table
# input:    runrecord txt file and path where the file resides
# output:   data frame
# note:        

# ROXYGEN Documentation
#' Read the runrecord
#' @description Read the run record into readable table. This function assumes runrecord has been run with PsN option -maxlvl=0. It is to be used in conjunction with process.runrec
#' @param filename filename, e.g. "AAinfo.txt"
#' @param path diretory where the runrecord resides
#' @return Data.frame with all information extracted from the runrecord.
#' @export
#' @seealso \code{\link{process.runrec}}
#' @examples
#' rr = read.runrec(filename = "AAruninfo.txt"
#' , path = system.file(package = 'qpToolkit','NONMEM')
#' )
#' rr
#' process.runrec(rr)

read.runrec = function(filename, path = getOption("nmDir"))
{
   if(missing(filename)) stop("filename must be included.")
  runrec = read.table(file=file.path(path,filename), sep=";", skip=5, header=FALSE)
  runrec = runrec[, 1:18]
  names(runrec) = c(
  	'Run','Ref','OFV','dOFV','CondNum','Minimization','CovStep','Label', 
  	'Description', 'StructMod', 'something','IIV','IOV', 'ResMod', 'Estimation','DataSet','nObs','nID')
  runrec = runrec[!is.na(runrec$Run),]
  return(runrec)
}


