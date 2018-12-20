# Startup code

.onLoad <- function(libname, pkgname){
  #check and set default options
  
  if(is.null(getOption("nmDir"))) options(nmDir=file.path(getwd(),"NONMEM"))
  if(getOption("stringsAsFactors")) {
    options(stringsAsFactors = F) 
  }
  
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(qP.welcome(Sys.getenv("USERNAME")))
  packageStartupMessage(paste("package path =", path.package("qpToolkit")))
  packageStartupMessage(paste(R.installation.validation()$IQ.message))
  packageStartupMessage("setting stringsAsFactors to FALSE as qPharmetra default")
  if(getOption("stringsAsFactors")) {
     options(stringsAsFactors = F) 
  }
  if(is.null(getOption("qpExampleDir"))) options(qpExampleDir=file.path(path.package("qpToolkit"),"NONMEM"))
}