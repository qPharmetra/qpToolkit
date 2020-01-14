# name:     full.names
# purpose:  swaps shorthand / abbreviated column names in data frame for full names (units) for labelling purposes
# input:    shorthand / abbreviate column names
# output:   column names written out in full
# note:     is used for demographics tables

# ROXYGEN Documentation
#' swaps shorthand / abbreviated column names in data frame for full names (units) for labelling purposes
#' @param x Shorthand / Abbreviate column names
#' @return Column names written out in full
#' @export
#' @examples
#' full.names(c("wt","race","age"))
full.names = function(x)
{
  swap(x,
                  c('wt','ht','bmi','age','sex','crcl','race'),
                  c("Weight (kg)","Height (cm)","BMI (kg/m2)","Age (yr)","Gender","Creatinine Clearance (mL/min)","Race")
  )
}

if(F)
{
  full.names(c("wt","race","age"))
}
