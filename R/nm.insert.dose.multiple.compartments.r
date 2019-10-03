#' Insert doses in other compartments
#' @description To model drug entry into the system from multiple absorption sites this function allows to turn a NONMEM datset into one with additional doses in differetn compartments.
#' @param data NONMEM data.frame, should contain variable 'CMT'
#' @param dose.in.cmt numeric double or vector for new CMT doses
#' @param quiet when TRUE (default) silences the messages returned
#' @param ... (unquoted) columns to sort the result by
#' @return A data.frame with input dataset supplied with additional doses
#' @export nm.insert.dose.multiple.compartments
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr bind_rows mutate arrange
#' @seealso \code{\link{nm.insert.evid2amt0}}
#' @examples
#' library(dplyr)
#' ## original data:
#' nmData = example.NONMEM.dataset(TIME=seq(0,24,4))
#' nmData$CMT = with(nmData, swap(EVID, 0:1, 2:1))
#' 
#' ## adding doses to compartments 3, 4 and 5
#' nmData3 = nm.insert.dose.multiple.compartments(data = nmData
#'                                                , dose.in.cmt=c(3,4,5)
#'                                                , ID, TIME, -EVID
#'                                                
#' )
#' tbl_df(nmData3[, Cs(ID,TIME,EVID,AMT,CMT,DV)])

nm.insert.dose.multiple.compartments = function(
  data, 
  dose.in.cmt = c(1,2), 
  ..., 
  quiet = TRUE
){
  my.dots = paste(
    unlist(
      lapply(
        lazyeval::lazy_dots(...), function(x) paste(as.character(x$expr),collapse="")
      )
    )
    ,collapse=", "
  )
  
  data.d2 = subset(data, EVID %in% c(1,3,4))
  data.d2 = data.d2[rep(seq(nrow(data.d2)), ea = length(dose.in.cmt)),] %>%
    mutate(CMT = rep(dose.in.cmt, times = nrow(.)/length(dose.in.cmt)))
  
  # gather data,  sort and drop the additional sorting index
  nmData = bind_rows(data, data.d2) %>%
    arrange(...)
  
  if(!quiet)
     message(paste("Doses placed in Compartments (CMT)",paste(dose.in.cmt, collapse=", ")))  
  
  if(nchar(my.dots)==0) {
     if(!quiet) message("No sorting was performed as no sorting variables were provided.")
  } else {
     if(!quiet) message(paste("Data sorted by", my.dots))
  }
  
  return(nmData)
}

