    # name:     get.diag.names
# purpose:  pulls the names of the THETA/OMEGA/SIGMA as they appear in the NONMEM output and covariance matri(x)(ces)
# input:    names
# output:   character vector
# note:

# ROXYGEN Documentation
#' Find diagonal OMEGA names
#' @description Retrieve which names in a vector of NONMEM parameter names are the diagonal ones. Thus, slect which ones are (1,1),, (2,2), etc... instead of (1,2), (3,4), etc...  The function is used internally by functions post-processing NONMEM output
#' @param names character vector with OMEGA and SIGMA names
#' @param sep separator - defaults to comma (e.g. OMEGA(1,1))
#' @return named vector of logicals; TRUE for giagonal, FALSE for off-diagonal elements
#' @export
#' @importFrom Hmisc unPaste
#' @examples
#' covMatrix = get.covmat(path = getOption("qpExampleDir"), run = "example2")
#' get.diag.names(names = sub(" +", "", trimSpace(names(covMatrix[[1]]))))
get.diag.names <- function(names, sep = ",")
{
  isOMEGA = grep("OMEGA",names)
  isSIGMA = grep("SIGMA",names)

  myOMEGANames = names[isOMEGA]
  myOMEGANames = gsub("[)]","", myOMEGANames)
  myOMEGANames = gsub("[(]","", myOMEGANames)
  myOMEGANames = gsub("OMEGA","", myOMEGANames)
  nams = names[isOMEGA]
  if(length(names[isOMEGA]) == 1) diagonal.omega = TRUE else {
    offDiag = apply(
      apply(
        sapply(myOMEGANames, function(x,sep)
          unlist(unPaste(x,sep = sep)),sep = sep
          ), 1, as.numeric
        ), 1, diff
    )
    diagonal.omega = offDiag == 0
  }
  names(diagonal.omega) = nams

  mySIGMANames = names[isSIGMA]
  mySIGMANames = gsub("[)]","", mySIGMANames)
  mySIGMANames = gsub("[(]","", mySIGMANames)
  mySIGMANames = gsub("SIGMA","", mySIGMANames)
  nams = names[isSIGMA]
  if(length(names[isSIGMA]) == 1) diagonal.sigma = TRUE else {
    offDiag = apply(apply(sapply(mySIGMANames, function(x,sep) unlist(Hmisc::unPaste(x,sep = sep)),sep = sep),1,as.numeric),1,diff)
    diagonal.sigma = offDiag == 0
  }
  names(diagonal.sigma) = nams

  return(list(OMEGA = diagonal.omega, SIGMA = diagonal.sigma))
}



