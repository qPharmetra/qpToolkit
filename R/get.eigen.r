
# ROXYGEN Documentation
#' NONMEM run Eigen values
#' @description Get the Eigen values from a NONMEM run. Requires the user to have run NONMEM with $COV ... PRINT=E option.
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param file.ext extension of the NONMEM output file (.lst)
#' @return list with named vectors with Eigen values for each estimation method
#' @export
#' @seealso \code{\link{get.nm.version}}, \code{\link{get.shrinkage}}, \code{\link{get.ofv}}, \code{\link{read.out}}
#' @examples
#' get.eigen(run = "example1", path = getOption("qpExampleDir"))
#' get.eigen(run = "example2", path = getOption("qpExampleDir"))

get.eigen = function(run, path = getOption("nmDir"), file.ext = ".lst")
{ 
  out = read.out(path=path, run=run, file.ext = file.ext)
  if(length(grep("EIGENVALUES",out))==0)
  {
    message("no eigen values. To obtain eigen values add PRINT=E to $COV in NONMEM")
    return(NULL)
  }
  txtStart = grep("EIGENVALUES", out)-1
  txtStop = which(out == " ")
  
  ## text to parse for Eigen values
  txtParse = as.list(seq(length(txtStart)))
  for(i in 1:length(txtStart))
  {
    tmp = txtStop - txtStart[i]
    tmp = tmp[tmp>0][2:3]
    txtParse[[i]] = (txtStart[i] + tmp[1] + 1) : (txtStart[i] + tmp[2] - 1)
  }
  eigen = lapply(txtParse, function(x, out) paste(out[x], collapse = ""), out = out)
  
  ## text to parse for the estimation method
  nameParse = as.list(seq(length(txtStart)))
  for(i in 1:length(txtStart))
  {
    tmp = txtStop - txtStart[i]
    tmp = tmp[tmp>0][1]
    nameParse[[i]] = (txtStart[i]) 
  }
  parName = lapply(nameParse, function(x, out) sub("[*]+","", out[x]), out = out)
  parName = unlist(lapply(parName, function(x) trimSpace(sub("[*]+","", x))))

  eigen = lapply(eigen, function(x) unlist(strsplit(trimSpace(x), " ")))
  eigen = lapply(eigen, function(x) asNumeric(x[x!=""]))
  names(eigen) = parName
  return(eigen)
}

