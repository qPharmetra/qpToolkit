
#' Title
#'
#' PK-PD superposition of multiple doses
#' @description Plots PK-PD predictions based on qP's suite of PK and Effect Model functions
#' @param doses numeric vector of doses
#' @param t.doses numeric vector of time of doses (equal length of \code{doses})
#' @param t.obs time opf observations
#' @param pk.func pharmacokinetic function to evaluate. See \code{\link{pk.pred}}
#' @param e.func effect model function to evaluate. See \code{\link{pk.pred}}
#' @param parms numeric vector (may be a named vector but is not required)
# @param log logical indicating to log the Y axis
#' @param output if TRUE the simulation input and outpout is returned as list. If FALSE (default) the plot is created.
#' @importFrom graphics plot lines segments text
#' @return A plot or a list with output
#' @export pkpdPredPlot
#'
#' @examples
#' eff.1comp.iv(dose = 100, tob = seq(0,24*7), parms = c(1,10, 0.25))
#' pk.pred(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
#'        pk.func = eff.1comp.iv, parms = c(1,10, 0.25))
#' pkpdPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
#'             pk.func = pk.1comp.iv, e.func = eff.1comp.iv,
#'             parms = c(1,10, 0.25))
pkpdPredPlot = function(doses, t.doses, t.obs, pk.func, e.func, parms, output = FALSE)
{
   t.obs = sort(unique(c(t.obs, t.doses)))
   ypr.c = pk.pred(doses, t.doses, t.obs, pk.func, parms)
   ypr.sd.c = pk.pred(doses[1], t.doses[1], t.obs, pk.func, parms)
   ypr.e = pk.pred(doses, t.doses, t.obs, e.func, parms)
   ypr.sd.e = pk.pred(doses[1], t.doses[1], t.obs, e.func, parms)

   if(!output){
   plot(rep(t.obs,4), c(ypr.c, ypr.sd.c, ypr.e, ypr.sd.e), type = "n", xlab = "Time (unit)"
        ,ylab = c("Concentration & Effect"))
   #text(-0.12*max(t.obs), 0.7*max(ypr.c), "Effect", col = blue[5],xpd=T,srt=90,cex =1.3)
   #text(-0.12*max(t.obs), 0.3*max(ypr.c), "Concentration", col = green[5], xpd=T,srt = 90,cex=1.3)

   ## plot PK
   lines(t.obs, ypr.c, col = green[7], lwd = 1.5)
   lines(t.obs[t.obs>t.doses[2]], ypr.sd.c[t.obs>t.doses[2]], lwd = 1.5, col = green[8], lty = 2)

   ## plot Effect
   lines(t.obs, ypr.e, col = blue[5], lwd = 2.5)
   lines(t.obs[t.obs>t.doses[2]], ypr.sd.e[t.obs>t.doses[2]], lwd = 2, col = blue[8], lty = 2)

   ## depict dose times
   for(i in 1:length(t.doses))
      segments(x0 = t.doses[i], x1 = t.doses[i], y0 = 0, y1 = ypr.c[t.obs == t.doses[i]], lty = 2)

   ## depict which function was used
   text(x = 0.075 * max(t.obs), y = 1.075* max(ypr.c),
        substring(deparse(substitute(pk.func)),4), col = "blue", xpd = TRUE)
   } else list(t.obs=t.obs, ypr.c=ypr.c, ypr.sd.c = ypr.sd.c, ypr.e = ypr.e, ypr.sd.e = ypr.sd.e,
                    doses = doses, t.doses=t.doses, pk.func = pk.func, e.func = e.func)
} ## pkpdPredPlot