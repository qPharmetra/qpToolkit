# name: match.data.frame
# purpose: compare two data frames (or lists) and provide feedback on what if any rows
#       are the same between them.
# input: two dataframes, the value to return if a row does *not* match (note it is concerned
#       to integer), and a vector of values that cannot be matched. Any value in x matching a
#       value in this vector is assigned the nomatch value (default is FALSE (equivalent to NULL))
# output: vector of row numbers that match. For rows that do not match an NA is returned

# ROXYGEN Documentation
#' Compare two data frames
#' @description Compare two data frames (or lists) and provide feedback on what if any rows
#' @param x data frame 1 for comparison
#' @param table data frame 2 for comparison
#' @param nomatch exactly like the agurment nomatch in \code{match}
#' @param incomparables exactly like the argument incomparables in \code{match}
#' @return Placeholder for return description
#' @export
#' @examples
#' temp1 = data.frame(a=11:13,b=21:23)
#' temp2 = data.frame(a=11:13,b=21:23)
#' temp3 = data.frame(a=11:13,b=41:43)
#' temp4 = data.frame(a=11:13,b=c(21,42,23))
#'
#' match.data.frame(x=temp1, table=temp2)
#' match.data.frame(x=temp1, table=temp3)
#' match.data.frame(x=temp1, table=temp4)
#' #[1] 1 2 3
#' #[1] NA NA NA
#' #[1]  1 NA  3
#'
#' match.data.frame(x=temp1, table=temp4, nomatch=18)
#' #[1]  1 18  3
#'
#' temp1 = list(x=11:13)
#' temp2 = list(x=11:13)
#' match.data.frame(x=temp1, table=temp2)
#' # 1 2 3
#'
#' temp1 = list(x = cars[,1], y = cars[,2])
#' temp2 = list(x = cars[,1], y = cars[,2])
#' match.data.frame(x=temp1, table=temp2)
#' #[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 17 19 20 21 22 23 24 
#' #25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#'
#' temp1 = data.frame(a=11:13,b=21:23)
#' temp2 = data.frame(a=11:14,b=21:24)# dataframes with different numbers of rows
#' 
#' match.data.frame(x=temp1, table=temp2)
#' #[1] 1 2 3
#'
#'
match.data.frame = function(x, table, nomatch = NA, incomparables = F)
{
  # x and table are data frames (or lists) of the same length
  # match rows of x against rows of table
  if(!identical(incomparables, F)) stop(
    "incomparables argument is not supported when x is a list")
  p <- length(x)
  if(!is.list(table) || length(table) != p)
    stop("table must be a list the same length as x")
  if(length(unique(unlist(lapply(x, length)))) > 1)
    warning("lengths of keys (variables in x) differ")
  if(length(unique(unlist(lapply(table, length)))) > 1)
    warning("lengths of keys (variables in table) differ")
  
  uniqueTable <- lapply(table, unique)
  f <- function(i, x, u)  match(x[[i]], u[[i]])
  x <- lapply(seq(p), f, x = x, u = uniqueTable)
  table <- lapply(seq(p), f, x = table, u = uniqueTable)
  
  # Convert each row of x and of table into a character string (sep=" ")
  match(do.call("paste", x), do.call("paste", table), nomatch = nomatch)
}
