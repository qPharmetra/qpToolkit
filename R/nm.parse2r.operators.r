#' Change NONMEM operators into R operators
#'
#' @param nmtext NONMEM text like 'DOSE.GT.10'.
#' @return Character string with an R expressions of the NONMEM input
#' @export nm.parse2r.operators
#' @note This will only with for simple operators but does not yet accomodate (nested) IF-THEN-ELSE blocks.
#' @examples
#' nm.parse2r.operators(c("DOSE.GT.10","AMT.LE.10","ID.EQ.1000","SEX!=0"))

nm.parse2r.operators = function(nmtext)
{
   ## find operators
   nmtext = gsub("[.]GT[.]",">", nmtext)
   nmtext = gsub("[.]GE[.]",">=", nmtext)
   nmtext = gsub("[.]LT[.]","<", nmtext)
   nmtext = gsub("[.]LE[.]","<=", nmtext)
   nmtext = gsub("[.]EQ[.]","==", nmtext)
   nmtext = gsub("[.]NE[.]","!=", nmtext)
   return(nmtext)
}
