# name: cfc
# purpose: read data elements off of the Windows clipboard and put into an R vector, matrix,
#       or data frame.
# input: an appropriate selection of data elements must have been copied to the Windows clipboard
#       (e.g., using ctrl+c), the type of object to create (vector, matrix, data frame), whether
#       x
# output: a vector, matrix, or data frame.
# note:

# ROXYGEN Documentation
#' Create a vector, matrix, or data frame from clipboard object
#' @param obj.type either one of 'data.frame' (default), 'vector' or 'matrix'
#' @param strings.as.factors logical to treat strings as factors
#' @param row.names determining row.names
#' @export


cfc = function(obj.type="data.frame",strings.as.factors=FALSE,row.names=NULL) {
  #copy from clipboard to R
  if (obj.type == "vector")  result = scan("clipboard", sep="\t")
  if (obj.type == "matrix")	result = as.matrix(read.table(file="clipboard",header=FALSE,sep="\t"))
  if (obj.type == "data.frame")	result = read.table(	file="clipboard",
                                                     header=TRUE,
                                                     sep="\t",
                                                     row.names=row.names,
                                                     stringsAsFactors=strings.as.factors)
  copy.from.clip = result
}

# Examples
if(F) {
  # to execute these examples, one must open the "cfc example data.xlsx" Excel file
  # in the following folder, and select the ranges below prior to execution of each step.
  # "Software\R-utils\trunk\Example Project\Main Development\WorkArea"

  # select range B3:B13 and press ctrl+c prior to executing the next line
  my.vector = cfc("vector")
  class(my.vector)
  #numeric
  my.vector
  #[1]  5 28 24 16 40 28 29  5  3 24 19

  # select range D3:H16
  my.matrix = cfc("matrix")
  class(my.matrix)
  #matrix
  my.matrix
  #        V1 V2 V3 V4 V5
  #   [1,]  5  7  7  7  4
  #   [2,]  3  5  4  7  7
  #   [3,]  9  8  6  2  2
  #   [4,]  2  8  5  7  6
  #   [5,]  4  8  4  4  6
  #   [6,]  1  1  9  9  8
  #   [7,]  7  6  3  6  8
  #   [8,]  3  4  8  9  9
  #   [9,]  1  6  3  9  4
  #   [10,]  7  2  1  3  7
  #   [11,]  4  9 10 10  9
  #   [12,]  4 10  2  5  8
  #   [13,]  9 10  1 10  4
  #   [14,]  1  3  8  6 10

  # select range J3:N16
  my.df = cfc() # note data.frame s the default
  class(my.df)
  #data.frame
  my.df
  #             a         b  c  d   e
  #   1       one  4.817759  2  7 2.1
  #   2      two   3.063275  4  9 2.7
  #   3     three 10.960907  1  6 1.8
  #   4      four  7.734828  2  7 2.1
  #   5      five  3.762724  8 13 3.9
  #   6       six  5.450341  4  9 2.7
  #   7     seven 10.889382 10 15 4.5
  #   8     eight 10.683585 10 15 4.5
  #   9      nine  7.284243  8 13 3.9
  #   10      ten 10.074675  2  7 2.1
  #   11   eleven  5.123461  6 11 3.3
  #   12   twelve  4.615106  6 11 3.3
  #   13 thirteen  3.409077  2  7 2.1


  # select range J3:N16
  my.df = cfc(row.names=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan2"))
  class(my.df)
  my.df
  #               a         b  c  d   e
  #   Jan       one  4.817759  2  7 2.1
  #   Feb      two   3.063275  4  9 2.7
  #   Mar     three 10.960907  1  6 1.8
  #   Apr      four  7.734828  2  7 2.1
  #   May      five  3.762724  8 13 3.9
  #   Jun       six  5.450341  4  9 2.7
  #   Jul     seven 10.889382 10 15 4.5
  #   Aug     eight 10.683585 10 15 4.5
  #   Sep      nine  7.284243  8 13 3.9
  #   Oct       ten 10.074675  2  7 2.1
  #   Nov    eleven  5.123461  6 11 3.3
  #   Dec    twelve  4.615106  6 11 3.3
  #   Jan2 thirteen  3.409077  2  7 2.1
}
