# name:     ctc
# purpose:  copy to clipboard
# input:    data.frame or matrix
# output:   data.frame or matrix (in the clipoard)
# note:     contents can be taken outside R using this function.

# ROXYGEN Documentation
#' Copy a data.frame or matrix to clipboard
#' @param x any R object to be copied
#' @export

ctc = function(x) {
  #copy from R to clipboard
  write.table(x,file="clipboard",sep="\t",row.names=FALSE)
}

if(F)
{
  testdf = data.frame(A=1:3,B=4:6,C=Cs(A,B,C))
  ctc(testdf)
}