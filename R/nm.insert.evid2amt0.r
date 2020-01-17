globalVariables('EVID')
#' Insert doses in other compartments
#' @description To model transit compartment absorption this function allows to turn a NONMEM datset into one EVID=2 & AMT=0 records before each dose given.
#' @param data NONMEM data.frame, should contain variables
#' @param quiet when TRUE (default) silences the messages returned
#' @param ... (unquoted) columns to sort the result by
#' @return Data frame with an EVID=2 and AMT=0 record preceding each AMT>0 record
#' @export nm.insert.evid2amt0
#' @importFrom lazyeval lazy_dots
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter mutate arrange bind_rows
#' @seealso \code{\link{nm.insert.dose.multiple.compartments}}
#' @examples
#' library(dplyr)
#' nmData = example.NONMEM.dataset(TIME=seq(0,24,4))
#' nmData2 = nm.insert.evid2amt0(data = nmData, ID, TIME, -EVID)
#' tbl_df(nmData2[, c('ID','TIME','EVID','AMT','DV')])
nm.insert.evid2amt0 = function(data, ..., quiet = TRUE)
{
  my.dots = paste(
    unlist(
      lapply(
        lazyeval::lazy_dots(...), function(x) paste(as.character(x$expr),collapse="")
      )
    )
    ,collapse=", "
  )

  data.d2 = data %>%
    filter(EVID %in% c(1,3,4)) %>%
    mutate(AMT = 0, EVID = 2)

  # gather data,  sort and drop the additional sorting index
  data = bind_rows(data, data.d2) %>%
    arrange(...)

  if(!quiet) message("EVID=2 and AMT=0 records added for each dosing record.")

  if(nchar(my.dots)==0) {
     if(!quiet) message("No sorting was performed as no sorting variables were provided.")
  } else {
    if(!quiet) message(paste("Data sorted by", my.dots))
  }

  return(data)
}

if(F)
{
  nmData2 = nm.insert.evid2amt0(nmData)
  nmData2 = nm.insert.evid2amt0(nmData,  quiet=TRUE, ID, TIME,-EVID)
}
