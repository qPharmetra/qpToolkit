
# ROXYGEN Documentation
#' Parse the information from the .ext file
#' @description Read and parse the  information embedded in the .ext file from a completed NONMEM run. In case of multiple \code{$EST} calls, these will be parsed one by one and included with their respective \code{$EST} names into the resulting list
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param extension file extension of the .ext file
#' @param quiet if TRUE no message will be returned.
#' #' @return A list with data.frames (as many as there where \code{$EST} calls in the NONMEM control stream) containing the .ext output
#' @seealso \code{\link{read.out}}
#' @export
#' @importFrom Hmisc unPaste
#' @examples
#' read.ext(run = "example1", path = getOption("qpExampleDir"))
#' ## now parse an output where multiple $EST calls have been performed
#' read.ext(run = "example1", path = getOption("qpExampleDir"), file.ext = ".mext")
#' ## a large one
#' read.ext(run = "example1", path = getOption("qpExampleDir"))

read.ext = function(run
                    , path = getOption("nmDir")
                    , file.ext = ".ext"
                    , quiet=TRUE
)
{
  x = invisible(scan(file = file.path(path, run, paste(run,gsub("[.]","",file.ext), sep="."))
           , what = "character"
           , sep = "\n"
           , quiet = quiet)
  )
  loc = grep("TABLE NO", x)
  idx = 0 ## idx is 1 in case we have only one estimation method
  if(length(loc) == 1) idx = 1
  loc = matrix(c(loc,c(loc[-1]-1,length(x))), nrow = 2, byrow = TRUE)
  loc
  
  ## turn this into a list
  
  x = apply(loc, 2, function(loc, x) x[loc[1]:loc[2]], x = x)
  if(idx == 1) ## special case for a single table
  {
    x = list(x[,1])
  }
  xnam = lapply(x, function(y) unPaste(y[1], sep = ":"))
  names(x) = lapply(xnam, function(x) substring(x[2], 2))
  
  ## turn each list into a data frame
  parse.table = function(y)
  {
    y = y[-1]
    
    ## take the names of the data frame and remove them from the 'numeric' grid
    dfnames = unlist(unPaste(substring(y[1],2), sep = "\\s+"))
    y = y[-1]
    
    yy = lapply(y, function(z) as.numeric(unPaste(substring(z,2), sep = "\\s+")))
    yy = do.call("rbind", yy)
    yy = data.frame(yy[, -1])
    names(yy) = dfnames
    return(yy)
  }
  
  ## return the tables
  return(lapply(x, parse.table)) 
}



