# name:     panel.nlme.vpc.pred
# purpose:  panel function for predicted lines / colored polygons of a vpc plot of an nlme object
# input:    regular panel input
# output:   regular panel output
# note:     not to be used as standalone. will be be called by nlme.vpcplot

#' Lattice function for predicted components of an nlme VPCs
#' @description  Panel function for predicted lines / colored polygons of a vpc plot of an nlme object
#' @param other a collection of parameters are available to modify the layout of the VPC
#' @return Lattice panel output (invisible)
#' @note Not to be used as standalone. Will be be called by \code{nlme.vpcplot}
#' @seealso \code{\link{nlme.vpcplot}}
#' @import lattice


panel.nlme.vpc.pred = function(x, y, OBS, vpc, subscripts, logY, showPredAs, showObsDots, showObsLines, xCov, 
                               col.scheme, obscex.dot, obspch.dot, ...)
{
  if(showPredAs == "lines")
  {
    yl = attr(y, "other")[,1]
    yh = attr(y, "other")[,2]
    yl2= attr(y, "other")[,3]
    yh2= attr(y, "other")[,4]
    if(logY==T) {yh = log10(yh); yl = log10(yl);yh2 = log10(yh2); yl2 = log10(yl2)}
    llines(x,yl, col = col.scheme$pred$outer,  type = "l", lwd = 1.5)
    llines(x,yh, col = col.scheme$pred$outer,  type = "l", lwd = 1,5)
    llines(x,yl2, col = col.scheme$pred$outer,  type = "l", lwd = 1.5)
    llines(x,yh2, col = col.scheme$pred$outer,  type = "l", lwd = 1.5)
    
    llines(x,y, ...,  type = "l", col = col.schem$pred$central, lwd = 3)
  }
  
  if(showPredAs == "area")
  {
    yl = attr(y, "other")[,1]
    yh = attr(y, "other")[,2]
    yl2= attr(y, "other")[,3]
    yh2= attr(y, "other")[,4]
    if(logY==T) {  yh = log10(ifelse(yh<=0, 0.01, yh)); yl = log10(ifelse(yl<=0, 0.01, yl));
                   yh2 = log10(ifelse(yh2<=0, 0.01, yh2)); yl2 = log10(ifelse(yl2<=0, 0.01, yl2))}
    lpolygon(c(x, rev(x)), c(yl, rev(yh)),  col = col.scheme$pred$area,  border = F)
    lpolygon(c(x, rev(x)), c(yl2,rev(yh2)), col = col.scheme$pred$inner, border = F)
    llines(x,y, ...,  type = "l", col = col.scheme$pred$central, lwd = 3)
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