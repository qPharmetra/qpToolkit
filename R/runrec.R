globalVariables(c(
   'Based on', 'CN', 'Desc', 'Description', 'FROM',
   'Min', 'OFV', 'Par', 'Ref', 'Run', 'dOFV', 'npar', 'ofv',
   'parameters', 'problem', 'run',  'symbol', 'val'
))
#' Construct a Run Record
#' 
#' Constructs a psn-style run record.
#' @param x vector of integers representing models
#' @param prefix character string to prepend to x
#' @param suffix character string to append to x
#' @param mods model names constructed from x, prefix, and suffix by default
#' @param ... ignored
#' @export
#' @keywords internal
#' @return decorated data.frame
#' @importFrom magrittr %$%
#' @importFrom dplyr transmute
#' 


runrec <- function(x, prefix = 'run', suffix = '', ...){
   #trail <- c(1:8, 12:14, 17:20, 24, 38, 39, 62, 63, 71)
   if(!requireNamespace('nonmemica', quietly = TRUE)){
      message('runrec() requires nonmemica; please install')
      return()
   }
   if(!requireNamespace('tidyr', quietly = TRUE)){
      message('runrec() requires tidyr; please install')
      return()
   }
   if(!requireNamespace('tidyselect', quietly = TRUE)){
      message('runrec() requires tidyselect; please install')
      return()
   }
   if(!requireNamespace('yamlet', quietly = TRUE)){
      message('runrec() requires yamlet; please install')
      return()
   }
   nms <- paste0(prefix, x, suffix)
   # sapply(nms, nm.unzip)
   mm <- nonmemica::as.model
   based_on <-  . %>% mm %$% problem %>% attr('runrecord') %$%`Based on`
   description <-  . %>% mm %$% problem %>% attr('runrecord') %$%`Description`
   
   ref <- sapply(nms, based_on)
   des <- sapply(nms, description)
   
   x <- parameters(nms)
   gthr <- tidyr::gather
   sprd <- tidyr::spread
   allof <- tidyselect::all_of
   
   x %<>% gthr(run, val, allof(nms), factor_key = T)
   x %<>% sprd(symbol, val)
   x %<>% left_join(data.frame(run = names(ref), ref = ref), by = 'run')
   x %<>% left_join(data.frame(run = names(des), Description = des), by = 'run')
   
   x %<>% mutate(ref = ifelse(ref == '', ref, paste0('run',ref)))
   
   .cnum <- function(x, ...){
      e <- get.eigen(x)
      if(is.null(e))e <- NA
      stopifnot(length(e) == 1)
      e <- e[[1]]
      hi <- max(e)
      lo <- min(e)
      cn <- hi/lo
      cn
   }
   
   cnum <- function(x, ...){
      sapply(x, .cnum, ...)
   }
   
   cn <- cnum(nms)
   cn <- data.frame(run = names(cn), cn)
   x %<>% left_join(cn, by = 'run')
   x %<>% mutate(cn = round(cn))
   x %<>% transmute(
      Run = suppressWarnings(nonmemica::text2decimal(run)),
      Ref = suppressWarnings(nonmemica::text2decimal(ref)),
      OFV = as.numeric(ofv),
      Par = as.numeric(npar),
      CN = cn,
      Min = min, 
      Desc = Description
   )
   x %<>% left_join(
      data.frame(
         Ref = x$Run,
         FROM = x$OFV
      ),
      by = 'Ref'
   )
   x %<>% mutate(dOFV = round(digits = 1, OFV - FROM))
   x %<>% select(Run, Ref, OFV, dOFV, Par, CN, Min, Desc)
   dcrt <- yamlet::decorate
   x %<>% dcrt('
Run: Run Number
Ref: Reference Run Number
OFV: Minimum Value of the Objective Function
dOFV: Change in OFV from Reference
Par: Number of Parameters
CN: Condition Number
Min: [ Minimization Status, [ Successful: 0, Failed: 1 ]]
Desc: Description of Feature(s)
')
   return(x)
}