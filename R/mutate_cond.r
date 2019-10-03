#' Conditional Mutate (dplyr) rows that match a specified condition
#' @description Mutate rows that match a specified condition, when using dplyr.
#' @param .data dataset to perform opertaion
#' @param condition condition to 
#' @param ... The mutate argument to execute over
#' @param envir environment - this parameter is best left alone, as the default setting allows use as standalone as well as in piping calls
#' @return the dataset with the conditional mutation
#' @export mutate_cond
#' @examples
#' library(magrittr)
#' library(dplyr)
#' nmData = example.NONMEM.dataset() %>% 
#'    mutate(CMT = swap(EVID, 0:1, 2:1)) %>%
#'    mutate_cond(condition = CMT>1
#'                , CMT = 99
#'    )
#' tbl_df(nmData)
   
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
   condition <- eval(substitute(condition), .data, envir)
   .data[condition, ] <- .data[condition, ] %>% mutate(...)
   .data
}
