#' Calculate Number of Parameters
#' 
#' Calculates the number of THETA, OMEGA and SIGMA 
#' parameters combined that are estimated (and not 
#' fixed) in the NONMEM model file.
#'
#' @param model select the model run number (e.g. 1)
#' @param path  select the directory where the model file resides
#' @param prefix text to precede run number in model name (default: 'run')
#' @param ...  additional arguments passed to \code{\link{nm.params.table}}
#' @return returns a vector with the number of estimated THETA, OMEGA and SIGMA parameters
#' @author Koen Jolling
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' models = c(101,102,104:109,111,115, 141:145, 147:149, 151, 153:156)
#' get.npar(models)
#' }}
#'
get.npar = function(model,path=getOption("nmDir"), prefix = 'run',...){
  
  fnct <- function(df)
  {
    length(df$Parameter[df$estimated=="estimated"])
  }
  
  model = paste(prefix,model,sep="")
  dsparam = list()
  out=list()
  for(i in model){
    txt <- paste(i,sep="")
    param = nm.params.table(run=i,path=path, ...)
    dsparam = list(param)
    names(dsparam) = paste(txt)
    P = lapply(dsparam, fnct)
    out=c(out,P)
  }
  return(unlist(out))
}

#' Calculate number of Thetas
#' 
#' Calculates the number of THETA parameters 
#' that are estimated (and not fixed) 
#' in the NONMEM model file.
#'
#' @param model  select the model run number (e.g. 1)
#' @param path   select the directory where the model file resides
#' @param prefix text to precede run number in model name (default: 'run')
#' @param ...    additional arguments passed to \code{\link{get.theta}}
#' @return       returns a vector with the number of estimated THETA parameters
#' @author       Koen Jolling
#' @export
#' @examples
#' \dontrun{\donttest{
#' models = c(101,102,104:109,111,115, 141:145, 147:149, 151, 153:156)
#' get.theta.npar(models)
#' }}
#'
#' 
get.theta.npar = function(model, path=getOption("nmDir"),prefix = 'run', ...){
  
  fnct <- function(df)
  {
    length(df$Parameter[df$estimated=="estimated"])
  }
  
  model = paste(prefix,model,sep="")
  dstheta = list()
  out=list()
  for(i in model){
    txt <- paste(i,sep="")
    theta = get.theta(run=i,path=path, ...)
    dstheta = list(theta)
    names(dstheta) = paste(txt)
    P = lapply(dstheta, fnct)
    out=c(out,P)
  }
  return(unlist(out))
}

#' Calculate Number of Omegas
#' 
#' Calculates the number of OMEGA parameters 
#' that are estimated (and not fixed) 
#' in the NONMEM model file.
#'
#' @param model  select the model run number (e.g. 1)
#' @param path   select the directory where the model file resides
#' @param prefix text to precede run number in model name (default: 'run')
#' @param ...    additional arguments passed to \code{\link{get.omega}}
#' @return       returns a vector with the number of estimated OMEGA parameters
#' @author       Koen Jolling
#' @export
#' @examples
#' 
#' \dontrun{\donttest{{
#' models = c(101,102,104:109,111,115, 141:145, 147:149, 151, 153:156)
#' get.omega.npar(models)
#' }}
#' 


get.omega.npar = function(model, path=getOption("nmDir"), prefix = 'run', ...){
  
  fnct <- function(df)
  {
    length(df$Parameter[df$estimated=="estimated"])
  }
  
  model = paste(prefix,model,sep="")
  dsomega = list()
  out=list()
  for(i in model){
    txt <- paste(i,sep="")
    omega = get.omega(run=i,path=path,...)
    dsomega = list(omega)
    names(dsomega) = paste(txt)
    P = lapply(dsomega, fnct)
    out=c(out,P)
  }
  return(unlist(out))
}

#' Calculate Number of Sigmas
#' 
#' Calculates the number of SIGMA parameters 
#' that are estimated (and not fixed) in 
#' the NONMEM model file.
#'
#' @param model   select the model run number (e.g. 1)
#' @param path    select the directory where the model file resides
#' @param prefix  text to precede run number in model name (default: 'run')
#' @param ...     additional arguments passed to \code{\link{get.sigma}}
#' @return        returns a vector with the number of estimated SIGMA parameters
#' @export
#' @author        Koen Jolling
#' @examples
#'
#'\dontrun{\donttest{
#' models = c(101,102,104:109,111,115, 141:145, 147:149, 151, 153:156)
#' get.sigma.npar(models)
#'}}
#' 

get.sigma.npar = function(model,path=getOption("nmDir"), prefix = 'run', ...){
  
  fnct <- function(df)
  {
    length(df$Parameter[df$estimated=="estimated"])
  }
  
  model = paste(prefix,model,sep="")
  dssigma = list()
  out=list()
  for(i in model){
    txt <- paste(i,sep="")
    sigma = get.sigma(run=i,path=path, ...)
    dssigma = list(sigma)
    names(dssigma) = paste(txt)
    P = lapply(dssigma, fnct)
    out=c(out,P)
  }
  return(unlist(out))
}
