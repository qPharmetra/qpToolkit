# name:     get.xpose.tables
# purpose:  load all Xpose tables and bind them together into one data frame
# input:    path of run directory where Xpose tables reside
# output:   data.frame
# note:

# ROXYGEN Documentation
#' Get NONMEM output tables into one data frame
#' @description Get NONMEM output tables in XPOSE format into one data frame. Requires all files to be created without the FIRSTONLY option in $TABLE. Also, ensure ONEHEADER option is consistently used in all $TABLE statements
#' @param run character vector of run roon names (e.g. run1)
#' @param path directory where run directories of runs reside
#' @return  data frame with all (unique) data items exported in Xpose tables sdtab, patab, catab and cotab
#' @export
#' @seealso \code{\link{get.multiple.xpose.tables}}
#' @examples
#' out.1 = get.xpose.tables(path = getOption("qpExampleDir"), run= "example2")
#' head(out.1)

get.xpose.tables = function(run, path = getOption("nmDir"))
{
  myPath = file.path(path,run)
  tabNames = dir(myPath)[grepl("(sd|pa|ca|co)tab+",dir(myPath))]
  read.nmTabs = function(path.in=myPath,tabNames.in=tabNames) {
    if(length(tabNames.in)>0) {
      nmTab = read.table(paste(path.in,tabNames.in[1],sep="/"),skip=1,header=TRUE)
      if(length(tabNames.in)>1) {
        for(tabName in tabNames[2:length(tabNames)]){
          tmp = read.table(paste(path.in,tabName,sep="/"),skip=1,header=TRUE)
          if(!all(!(names(tmp)%in%names(nmTab)) == FALSE))
            ## check if there is something to add
           nmTab = cbind(nmTab,tmp[,!(names(tmp)%in%names(nmTab))])
            ## if yes add it, if not go to next
        }
      }
    }
    return(nmTab)
  }
 return(read.nmTabs())
}

if(F)
{
  out116 = get.xpose.tables(path = file.path(nmDir, "run116"))
  head(out116)
}
