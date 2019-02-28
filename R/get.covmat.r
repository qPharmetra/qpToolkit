# name: get.covmat
# purpose: get the covariance matrix from a NONMEM run which has successfully generated a
#       covariance matrix.
# input: the name of the NONMEM run, which should be held in a same-named folder within
#       the standard qp NONMEM folder for a project
# output: a named list. The sole item in the list should have a name that is the
#       estimation method used (e.g. "First Order Conditional Estimation with Interaction").
#       That named item will be a data frame containing the covariance matrix.


# ROXYGEN Documentation
#' Get the covariance matrix
#' @description Get the covariance matrix from a completed NONMEM run. If the the run has been zipped, the file will be extracted, then parsed and thereafter the file (not the zipped file) will be discarded.
#' @param run run rootname (e.g. run1)
#' @param path directory where run output resides
#' @param ext extension of the covariance 
#' @return list with covariance matrices for each estimation method
#' @export
#' @seealso \code{\link{nm.covmat.extract}}, \code{\link{extract.varcov}}
#' @examples
#' get.covmat("example2", path = getOption("qpExampleDir"))

get.covmat = function(run, path = paste(getwd(),"NONMEM", sep="/"), ext = "cov")
{
  check.unzipped = FALSE
  covFileName = paste(path,run,paste(run,ext,sep="."),sep="/")
  zipFileName = paste(path,run,paste(run,ext,"7z",sep="."),sep="/")
  if(!file.exists(covFileName))
  {
    if(file.exists(zipFileName))
    {
      check.unzipped = TRUE
      nm.unzip(run = run, extension = paste(".",ext,sep=""), path = paste(path, run, sep="/"))
    }
  }
  out = NULL
  if(file.exists(covFileName))
  {
    if(ext == "cov") out = nm.covmat.extract(run = run, path = path)
    if(ext == "cor") out = nm.cormat.extract(run = run, path = path)
    if(check.unzipped) file.remove(covFileName)
  }
  return(out)
}

# Examples
if (F) {
  get.covmat("run116")
  #path: C:/Work/Software/R-utils/trunk/TestArea/NONMEM/run116
  #call: c:/progra~1/7-zip/7z e run116.cov.7z
  #
  #7-Zip [64] 9.20  Copyright (c) 1999-2010 Igor Pavlov  2010-11-18
  #
  #Processing archive: run116.cov.7z
  #
  #Extracting  run116.cov
  #
  #Everything is Ok
  #
  #Size:       2046
  #Compressed: 618
  #$`First Order Conditional Estimation with Interaction`
  #             THETA1       THETA2       THETA3       THETA4       THETA5       THETA6       THETA7       SIGMA(1,1)   OMEGA(1,1)   OMEGA(2,1)   OMEGA(2,2)
  #THETA1        0.039587100     4.152870  3.19958e-04 -3.79808e-04            0  4.70108e-04  0.009334790            0 -3.22093e-04            0  0.002681120
  #...
  #OMEGA(2,2)    0.002681120     5.550180  1.91312e-03 -1.97426e-04            0 -3.18239e-04  0.000914467            0  9.40787e-04            0  0.012760800

  get.covmat("run118")
  #...
  #$`First Order Conditional Estimation with Interaction`
  #             THETA1       THETA2       THETA3       THETA4       THETA5       THETA6       THETA7       THETA8       THETA9       SIGMA(1,1)   OMEGA(1,1)   OMEGA(2,1)
  #THETA1        0.063226400    5.6722100  8.40495e-04 -4.34006e-04            0  3.39546e-04  0.013611800 -3.75462e-03 -2.00030e-03            0 -1.47336e-04            0
  #...
  #OMEGA(2,2)    0.011386500
  
  # NOTE the following produces JUST the matrix, not the
  x = get.covmat("run118")
  names(x)
  #"First Order Conditional Estimation with Interaction"
  class(x)
  #list
  x$`First Order Conditional Estimation with Interaction`
  #>   x$`First Order Conditional Estimation with Interaction`
  #             THETA1       THETA2       THETA3       THETA4       THETA5       THETA6       THETA7       THETA8       THETA9       SIGMA(1,1)   OMEGA(1,1)   OMEGA(2,1)
  #THETA1        0.063226400    5.6722100  8.40495e-04 -4.34006e-04            0  3.39546e-04  0.013611800 -3.75462e-03 -2.00030e-03            0 -1.47336e-04            0
  #THETA2        5.672210000 3392.1800000  1.13212e+00 -1.22182e-01            0 -1.43966e-01  3.469120000 -8.74302e-02 -2.61601e-01            0  3.28763e-01
  
  class(x$`First Order Conditional Estimation with Interaction`)
  #data.frame
}