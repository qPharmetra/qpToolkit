# ROXYGEN Documentation
#' Detect differences between two data.frames
#' @description given two data frames, which if any column in the first is not found in the second
#' @param df.1 first data.frame
#' @param df.2 second data.frame
#' @return Placeholder for return description
#' @export
#' @examples
#' my.df.1 = data.frame(a = 1:3,b = 4:6,c = 7:9)
#' my.df.2 = data.frame(a = 1:3,b = 4:6)
#'
#' miss.col(my.df.1,my.df.2)
#' #"c"
#' miss.col(my.df.2,my.df.1)
#' #character(0)
miss.col <- function(df.1, df.2) {

  x = names(df.1)
  x[x %nin% names(df.2)]
}
