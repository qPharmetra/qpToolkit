# name:     read.bootstrap
# purpose:  reads summary files of a completed NONMEM bootstrap
# input:    path of bootstrap file location, the filenames for the raw_results and the results.csv files
# output:   list of bootstrap results
# note:     

# ROXYGEN Documentation
#' Documentation stub
#' @description Reads summary files of a completed NONMEM bootstrap
#' @param path Directory where bootstrap summary files reside
#' @param filename default Filename of the bootstrap results summary file created by PsN bootstrap
#' @param structure.filename Filename of structure file created by PsN bootstrap
#' @param parlist Define what elements are needed from the bootstrap?
#' @return A list with data frame withy summary boostrap results
#' @export
#' @importFrom Hmisc unPaste
#' @examples
#' \dontrun{
#' myBoot = read.bootstrap(path = getOption("qpExampleDir")
#'   , filename = "raw_results_bs4011.csv"
#'   , structure.filename = "raw_results_structure"
#'   )
#' bootstrap.ParTab(myBoot, idx = list(theta=1:13,omega=1:7,sigma=1))
#' names(myBoot)
#' str(myBoot$bootstrap)
#' myBoot$structure
#' }
#' 
read.bootstrap = function(
  path =getOption("nmDir"),       # where do the bootstrap results reside?
  filename   = "raw_results1.csv",  # default filename for bootstrap results
  structure.filename = "raw_results_structure", ## contains 
  parlist = c("ofv","theta","omega")# what elements do you want from bootstrap?
)
{
  default.names = c("model", "minimization.successful","covariance.step.successful","covariance.step.warnings","estimate.near.boundary")
  
  ## read bootstrap raw results
  bootstrap.data <- read.csv(file.path(path, filename), header=TRUE)
  ## replace underscores, brackets and commas
  for (i in 1:length(names(bootstrap.data))) {
    names(bootstrap.data)[i] <- gsub("\\_", "\\.", names(bootstrap.data)[i]) 
  }
  
  ## read and process bootstrap structure file
  struct = scan(file=paste(path,structure.filename, sep="/"),
                what="character",sep=  "\n", quiet=TRUE)
  struct = struct[2:(which(substring(struct,1,3)=="[1]")-1)]
  struct = struct[substring(struct, 1,12) != "line_numbers"]
  struct = unPaste(struct,sep = "=")
  struct.names = gsub("\\_", "\\.",struct[[1]])
  struct.loc = lapply(unPaste(struct[[2]],sep = ","), asNumeric)
  struct.loc[[1]] = struct.loc[[1]] + 1
  
  bootlist = lapply(seq(along=struct.names), 
                    function(i, struct.names, struct.loc, bootdf){  #i=1;i=31
                      mySequence = struct.loc[[1]][i]:(struct.loc[[1]][i]+struct.loc[[2]][i]-1)
                      theBootComponent = if(length(mySequence)>1){
                        as.data.frame(bootdf[, mySequence])
                      } else {
                        data.frame(value = bootdf[, mySequence])
                      }
                      if(length(mySequence) == 1) names(theBootComponent) = struct.names[i]
                      theBootComponent
                    }, struct.names = struct.names, struct.loc = struct.loc, bootdf = bootstrap.data)
  names(bootlist) = struct.names
  
  myBootDF = do.call("cbind", lapply(c(default.names,parlist), 
                                     function(x, bootlist) bootlist[[x]], bootlist = bootlist))
  myBootDF = data.frame(as.matrix(myBootDF))
  names(myBootDF)
  
  ## assign the proper names to myBootDF
  myMatch = match(c(default.names,parlist), struct.names)
  myStructure = data.frame(component = c(default.names,parlist),
                           start = struct.loc[[1]][myMatch], end =  struct.loc[[2]][myMatch])
  
  cat("read", nrow(myBootDF)-1, "bootstrap replicates\n")
  return(list(bootstrap = myBootDF, structure = myStructure, path = path))
}

if(F)
{
  
  myBoot = read.bootstrap(path=paste(getwd(),"NONMEM/bootstrap/bs4011", sep = "/"),
                          filename = "raw_results_bs4011.csv",
                          structure.filename = "raw_results_structure")
  names(myBoot)
  str(myBoot$bootstrap)
  myBoot$structure
}
