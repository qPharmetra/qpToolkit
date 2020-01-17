# name:     expand.data
# purpose:  multiply data length(values) times and left join the values as a column
# input:    data.frame, character or numeric vector (values), and name (column name)
# output:   data.frame
# note:

# ROXYGEN Documentation
#' Multiply data.frames
#' @description Multiply data length(values) times and left join the values as a column
#' @param data input data set
#' @param values values of the new column to be added
#' @param name name of the column to be added
#' @export
#' @examples
#' input.df = data.frame(A=1:2,B=2:3,C=4:5)
#' input.df
#' expand.data(input.df, values = c(1,3,7), "Klaas")
expand.data <- function(data, values, name = "variable.added")
{
  ## building on expand.grid functionality
  ## multiply data length(values) times and left join the values as a column
  if(!is.vector(values)) {message("argument 'values' must be a vector."); return()}
  values = sunique(values)
  len = length(values)
  newdata = data[rep(1:nrow(data), times = len), ]
  newdata$newVar = rep(values, ea = nrow(data))
  names(newdata)[names(newdata) == "newVar"] = name
  return(newdata)
}

