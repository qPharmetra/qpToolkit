# name: logTicks
# purpose: create a vector suitable for using as axis tickmarks in a graphic with a logged
#       axis (base 10), and specify a multiplier to adjust these further.
# input: vector of numbers, representing the limits of data to be displayed on axis.
#       There can be >1 value in vector in which case highest and lowest values are used
#       as limits, and "loc" which is a multiplier (see examples).
# output: vector of log base 10 values to be used for axis tickmarks, not intended to be used as standalone function

# ROXYGEN Documentation
#' Define ticks for plots with log10 axes
#' @description Create a vector suitable for using as axis tickmarks in a graphic with a logged
#       axis (base 10), and specify a multiplier to adjust these further.
#' @param lim the values to log base 10
#' @param loc a multiplier (see examples)
#' @return Vector of log base 10 values to be used for axis tickmarks, not intended to be used as standalone function
#' @export
#' @note This function is called by the scale functions for lo10 axes and is not to be used as standalone function.
#' @seealso \code{\link{xscale.components.log10}} and \code{\link{yscale.components.log10}}
#' @examples
#' logTicks(lim = c(10,100), loc = 1)
#' logTicks(lim = c(100,10), loc = 1)
#' logTicks(lim = c(4,109,1000), loc = 1)
#' logTicks(lim = c(4,109,999), loc = 1)
#' logTicks(lim = c(109,109,1000,4), loc = 7)
#' logTicks(lim = c(109,109,1000,4), loc = 4)
#' logTicks(lim = c(109,109,1000,4), loc = 2)
#' logTicks(c(2.,5,10,23,532,140,1240,0.2,5,50), loc = c(1,3))
logTicks = function(lim, loc = 1)
{
  lim = lim[lim>0] # make sure it ignores the zeros
  ii = floor(range(log10(lim)) + c(-1,2))
  mn = 10^(seq(ii[1],ii[2]))
  r = as.numeric(outer(loc, mn, "*"))
  r = r[min(lim) <= r & r <= max(lim)]  # ensure result has no values below or above the vector entered
  len = length(r)
  theSeq = if(len%%2 == 0) seq(1,len,length=5) else (seq(1,len,length=6))
  unique(r[round(theSeq)])
}

logTicks(lim = c(10,100), loc = 1)
logTicks(lim = c(100,10), loc = 1)
logTicks(lim = c(4,109,1000), loc = 1)
logTicks(lim = c(4,109,999), loc = 1)
logTicks(lim = c(109,109,1000,4), loc = 7)
logTicks(lim = c(109,109,1000,4), loc = 4)
logTicks(lim = c(109,109,1000,4), loc = 2)
