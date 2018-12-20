#' R Installation Validation
#'
#' @return Information
#' @import utils

R.installation.validation = function()
{
   find.in.path = function(path,fname){
      parts = unlist(strsplit(path, .Platform$file.sep))
      if(length(parts)==0) return("") #just return empty directory
      if(!file.exists(file.path(path,fname))) 
         find.in.path(do.call(file.path, as.list(parts[1:length(parts) - 1])),fname)
      else path
   }
   
   ## check R version, against project requirement, or against dependency requirement
   current.R = getRversion()
   
   wd = getwd()

   infoFile = file.path(find.in.path(wd,".info"),".info")
   if(file.exists(infoFile))
      {
      required.R = as.data.frame(read.dcf(infoFile))$Rversion
      IQ.Rversion = (current.R != required.R)
   } else{
      # this check is greater than
      required.R = tools::package.dependencies(utils::installed.packages()["qpToolkit",],depLevel="Depends")$qpToolkit #pull this from description file
      required.R = required.R[which(required.R[,1]=="R"),3]
      IQ.Rversion = (current.R >= required.R)
   }
   
   ## load R object with installed versions
   current.packs = utils::installed.packages()
   rownames(current.packs)=NULL
   current.packs = data.frame(current.packs)
   current.packs = subset(current.packs, !Priority %in% c("base","recommended"), 
                          select=c("Package","LibPath","Version"))
   # remove packrat from current so we don't check it
   current.packs = subset(current.packs,Package!="packrat")
   allowed.packs = read.csv(system.file("Qualification/package.versions.csv", package="qpToolkit"))
   # which R version (major.minor only) column to read
   allowed.packs$Version = allowed.packs[,paste("Vers",current.R$major,
                                                current.R$minor,sep=".")]
   allowed.packs = allowed.packs[,c("Package","Version")]
   
   #compare versions of packages installed, vs. what was in qpRepos during release of this qpToolkit version
   compare.packs = merge(current.packs,allowed.packs,suffixes=c(".current",".allowed"),by="Package",all.x=T)
   
   ## ----- Jason to create this and change the line above ---- ##
   compare.packs$non.approved = is.na(compare.packs$Version.allowed)
   compare.packs$bad.version = compare.packs$Version.current!=compare.packs$Version.allowed
   IQ.pack.versions = sum(compare.packs$bad.version,na.rm=T)>0
   IQ.pack.extras = sum(compare.packs$non.approved)>0
   
   IQ.message = ifelse(IQ.Rversion | IQ.pack.versions,"IQ FAILED","IQ PASSED")
   if(IQ.Rversion) IQ.message = paste(IQ.message, "Incorrect R Version",sep="\n")
   if(IQ.pack.versions) IQ.message = paste(IQ.message, "Incorrect Package Versions",sep="\n")
   if(IQ.pack.extras) IQ.message = paste(IQ.message, "Warning: non IQ compliant packages detected",sep="\n")
   
   return(list(IQ.message = IQ.message, packages.df = compare.packs))
   
}
   