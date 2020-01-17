# name:     panel.nlme.vpc.pred
# purpose:  panel function for predicted lines / colored polygons of a vpc plot of an nlme object
# input:    regular panel input
# output:   regular panel output
# note:     not to be used as standalone. will be be called by nlme.vpcplot

# ROXYGEN Documentation
#' Lattice function for observed components of an nlme VPCs
#' @description  Panel function for observed lines / colored polygons of a vpc plot of an nlme object
# @param other a collection of parameters are available to modify the layout of the VPC
#' @param x x variable
#' @param y y variable
#' @param logY unused
#' @param showPredAs unused
#' @param showObsDots whether to show observations as dots
#' @param showObsLines whether to show observations as lines
#' @param col.scheme list with elements: [obs: [dot, line]]
#' @param obscex.dot cex for observations
#' @param obspch.dot pch for observations
#' @param \dots passed to panel.xyplot()
#' @return Nothing -> internal to a lattice call
#' @note Not to be used as standalone. Will be be called by \code{nlme.vpcplot}
#' @seealso \code{\link{nlme.vpcplot}}
#' @import lattice
#' @importFrom Hmisc summarize
#' @importFrom Hmisc smedian.hilow

panel.nlme.vpc.obs = function(x, y, logY, showPredAs, showObsDots, showObsLines,
                              col.scheme, obscex.dot, obspch.dot, ...)
{
  if(showObsDots ) panel.xyplot(x,y, ..., cex = obscex.dot, pch = obspch.dot, col = col.scheme$obs$dot, type = 'p')
  if(showObsLines)
  {
    yy = Hmisc::summarize(y, list(x = x), smedian.hilow, stat.name = "central")
    llines(yy$x, yy$central,  col = col.scheme$obs$line, lwd = 2.5)
    llines(yy$x, yy$Lower,  col = col.scheme$obs$line, lty = 2)
    llines(yy$x, yy$Upper,  col = col.scheme$obs$line, lty = 2)
  }
}

if(F)
{
  EFF.1comp.1abs = function(dose, tob, cl, v, ka, keo)
  {
    # Effect-site concentration for 1-compartment model, 1st-order absorption

    kel = cl / v

    # Define coefficients
    A = 1/(kel-ka) / (keo-ka)
    B = 1/(ka-kel) / (keo-kel)
    C = 1/(ka-keo) / (kel-keo)

    # Return effect-site concentration
    dose*ka*keo/v * (A*exp(-ka*tob) + B*exp(-kel*tob) + C*exp(-keo*tob))
  }
  fit.PD004.nlme = nlme.run(model = value ~ base + EFF.1comp.1abs(dose, time, cl * exp(cl.eta), v, ka, keo),
                            data = pkpdData[pkpdData$type == "PD" & pkpdData$dose > 0 & pkpdData$value > 0.5, ],
                            fixed = base + cl + v + ka + keo ~ 1,
                            random = cl.eta ~ 1,
                            groups = ~ id,
                            start = c(base = 1, cl = 1, v = 10, ka = 1, keo = 0.01),
                            problem = "True Model",
                            reference = 4)
  summary(fit.PD004.nlme$object)
  nlme.extract(fit.PD004.nlme$object)$table
  vpc.PD004.nlme = nlme.vpc(fit.PD004.nlme$object, nrep = 100)
  nlme.vpcplot(fit.PD004.nlme$object, vpc.PD004.nlme)
}
