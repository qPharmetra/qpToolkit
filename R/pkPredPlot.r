# name:     pkPredPlot
# purpose:  plots PK predictions based on qP's suite of PK functions
# input:    doses, time of doses, tie of observations and the pk function
# output:   plot with superposed single dose and multiple doses PK predictions
# note:

# ROXYGEN Documentation
#' PK superposition of multiple doses
#' @description Plots PK predictions based on qP's suite of PK functions
#' @param doses numeric vector of doses
#' @param t.doses numeric vector of time of doses (equal length of \code{doses})
#' @param t.obs time opf observations
#' @param pk.func pharmcokinetic function to evaluate. See \code{\link{pk.pred}}
#' @param parms numeric vector (may be a named vector but is not required) with
#' @param log logical indicating to log the Y axis
#' @param output if TRUE the simulation input and outpout is returned as list. If FALSE (default) the plot is created.
#' @return PK plot
#' @seealso \code{\link{pk.pred}}, \code{\link{pk.1comp.iv}}, \code{\link{pk.1comp.0abs}}, \code{\link{pk.1comp.1abs}}, \code{\link{pk.1comp.1abs.ss}}, \code{\link{pk.1comp.10abs}}, \code{\link{pk.1comp.lag}}, \code{\link{pk.2comp.iv}}, \code{\link{pk.2comp.0abs}}, \code{\link{pk.2comp.1abs}}, \code{\link{pk.2comp.1abs.m}}, \code{\link{pk.2comp.1abs.ss}}, \code{\link{pk.3comp.iv}}, \code{\link{pk.3comp.iv.ss}}, \code{\link{pk.3comp.1abs}}, \code{\link{pk.3comp.1abs.ss}}, \code{\link{eff.1comp.iv}}, \code{\link{eff.1comp.1abs}}, \code{\link{eff.1comp.1abs.ss}}, \code{\link{eff.2comp.iv}}, \code{\link{eff.2comp.1abs}}, \code{\link{eff.2comp.1abs.ss}}, \code{\link{eff.3comp.1abs}}
#' @export
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @importFrom graphics segments
#' @importFrom graphics text
#' @examples
#' # 1comp elimination, 1st-order absorption
#' pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length = 7), t.obs = seq(0,24*7)
#'    , pk.func = pk.1comp.1abs, parms = c(1,25, 0.1, 5,0.5)
#' )
#'
#' # 2comp elimination, 0-order absoorption
#' pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length = 7), t.obs = seq(0,24*7)
#'    , pk.func = pk.2comp.0abs, parms = c(10,30, 3, 90,10), log = TRUE
#' )
#'
#' ## demo effect prediction after single and multiple doses
#' eff.1comp.1abs(dose = 100, tob = seq(0,24*7), parms = c(1,10, 0.25, 0.05))
#' pk.pred(doses = rep(100, 7), t.doses = seq(0,24*7,length = 7), t.obs = seq(0,24*7)
#'    , pk.func = eff.1comp.iv, parms = c(1,10, 0.25)
#' )
#' pkpdPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length = 7), t.obs = seq(0,24*7)
#'    , pk.func = pk.1comp.1abs, e.func = eff.1comp.1abs
#'    , parms = c(1,10, 0.25, 0.05)
#' )

pkPredPlot <- function(doses, t.doses, t.obs, pk.func, parms, log = FALSE, output = FALSE)
{
  t.obs = sort(unique(c(t.obs, t.doses)))
  ypr = pk.pred(doses, t.doses, t.obs, pk.func, parms)
  ypr.sd = pk.pred(doses[1], t.doses[1], t.obs, pk.func, parms)
  doLog = ""
  if(log) doLog = "y"
  if(!output){
     plot(t.obs, ypr, type = "l", log = doLog, xlab = "Time (unit)",ylab = "Concentration")
  lines(t.obs[t.obs>t.doses[2]], ypr.sd[t.obs>t.doses[2]], col = "red", lty = 2)
  for(i in 1:length(t.doses))
    segments(x0 = t.doses[i], x1 = t.doses[i], y0 = 0, y1 = ypr[t.obs == t.doses[i]], lty = 2)
  text(x = 0.075 * max(t.obs), y = 1.075* max(ypr)
       , deparse(substitute(pk.func)), col = qp.blue, xpd = TRUE
  )
  } else list(t.obs = t.obs, ypr = ypr, ypr.sd = ypr.sd
              , doses = doses, t.doses = t.doses, pk.func = pk.func)
}

