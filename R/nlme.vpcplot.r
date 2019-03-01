# name:     nlme.vpcplot
# purpose:  plots an nlme.vpc object
# input:    nlme.object, covariates to run VPC across
# output:   list with nlme object, the covariates inputted, the vpc results summarized and (optional) the vpc samples
# note:     requires input from nlme.vpct      

# ROXYGEN Documentation
#' Plot a VPC of an nlme object
#' @param object the nlme object (if it's an nlme.run object the input should read \code{object$object}) 
#' @param vpc the output from \code{nlme.vpc} 
#' @param formula a formula detailing what to plot
#' @param obsData the observation data, if not supplied defaults to \code{obsData = getData(object)}. Use if newdata in extract.nlme was specified
#' @param xLabel same as xlab
#' @param yLabel same as ylab
#' @param xLimits same as xlim
#' @param yLimits same as ylim 
#' @param logY logical to log Y axis
#' @param color.style list with layout options, such as:
#' @param symbol list with elements \code{pch} (15) and \code{cex} (0.5)
#' @param aspect trellis banking value (defaults to 1 for sqaure panels)
#' @param cex.label tex size for x and y labels (1.5)
#' @param nx number of bins for x covariate
#' @param showPredAs layout style (defaults to "area" for polygons but could be 'lines' too).
#' @param showObsDots logical to showobserved dots (T) 
#' @param showObsLines logical to show quantile lines of observed (T)
#' @param obscol.dot color of observed dot
#' @param obscex.dot symbol size
#' @param obspch.dot type of symbol
#' @param obscol.line observed line color
#' @param predcol.central predicted central tendency color
#' @param predcol.outer predicted outer polygon color
#' @param predcol.area predicted overall polygon color
#' @param predcol.inner predicted inner polygon (IQR) color
#' @param ... any additional arguments passed on to the lattice call
#' @return A VPC plot.
#' @export nlme.vpcplot
#' @seealso \code{\link{nlme.vpc}}, \code{\link{nlme.extract}}
#' @import nlme Hmisc lattice
#' @examples
#' library(nlme)
#' pkpdData = example.pkpdData()
#' EFF.1comp.1abs = function(dose, tob, cl, v, ka, keo)
#'  {
#'    # Effect-site concentration for 1-compartment model, 1st-order absorption
#'    
#'    kel = cl / v
#'    
#'    # Define coefficients
#'    A = 1/(kel-ka) / (keo-ka)
#'    B = 1/(ka-kel) / (keo-kel)
#'    C = 1/(ka-keo) / (kel-keo)
#'    
#'    # Return effect-site concentration
#'    dose*ka*keo/v * (A*exp(-ka*tob) + B*exp(-kel*tob) + C*exp(-keo*tob))
#'  }
#'  fit.PD004.nlme = nlme.run(model = value ~ base + EFF.1comp.1abs(dose, time, cl * exp(cl.eta), v, ka, keo), 
#'                            data = pkpdData[pkpdData$type == "PD" & pkpdData$dose > 0 & pkpdData$value > 0.5, ], 
#'                            fixed = base + cl + v + ka + keo ~ 1, 
#'                            random = cl.eta ~ 1, 
#'                            groups = ~ id, 
#'                            start = c(base = 1, cl = 1, v = 10, ka = 1, keo = 0.01),
#'                            problem = "True Model",
#'                            reference = 4)
#'  summary(fit.PD004.nlme$object)
#'  nlme.extract(fit.PD004.nlme$object)$table
#'  vpc.PD004.nlme = nlme.vpc(fit.PD004.nlme$object, nrep = 100)
#'  nlme.vpcplot(fit.PD004.nlme$object, vpc.PD004.nlme)

nlme.vpcplot = function(object, 
                        vpc, 
                        formula,
                        obsData = NULL, 	## defaults to obsData = getData(object)
                        ## use if newdata in extract.nlme was specified
                        xLabel, yLabel, 
                        xLimits, yLimits, 
                        logY = FALSE,
                        color.style = list(observed = rgb(0.2,0.2,0.2), central = blue[10], inner = blue[4], outer = blue[1]),
                        symbol = list(pch = 15, cex = 0.5),
                        aspect = 1, cex.label = 1.5, 
                        nx = NULL,
                        showPredAs = "area",
                        showObsDots = TRUE,
                        showObsLines = TRUE,
                        obscol.dot = gray[8], obscex.dot = 0.5, obspch.dot = 1,
                        obscol.line = gray[10],
                        predcol.central = blue[6],
                        predcol.outer = blue[7],
                        predcol.area = blue[1],
                        predcol.inner = blue[3],
                        ...
                        
                        ## must add option to summarize observed (as lines) instead of or on top of raw data dots
                        
)
{
  options(warn = -1)
  
  ## turn individual layout options into a list
  #showObsDots = T; showObsLines = T;obscol.dot = gray[8];obscex.dot = 0.5;obspch.dot = 1;obscol.line = gray[10];predcol.central = blue[6]
  #predcol.outer = blue[7];predcol.area = blue[1];predcol.inner = blue[3];
  
  col.scheme = list(obs = list(dot = obscol.dot, line = obscol.line), 
                    pred = list(central = predcol.central, outer = predcol.outer, area = predcol.area, inner = predcol.inner)) 
  
  myScales = list(cex = 0.8)
  myComponents = yscale.components.default
  
  if(logY){
    myScales = list(x = list(cex = 0.8), y = list(cex = 0.8, log = 10))
    myComponents = yscale.components.log10
  }
  
  xRan = c(0.98, 1.02) * range(getData(object)[, vpc$covariates[1]])
  observations = eval(as.name(getResponseFormula(object)[[2]]), getData(object))
  yRan = c(0.98, 1.02) * range(c(observations, vpc$vpc.summaryLower, vpc$vpc.summary$Upper))
  
  if(missing(yLimits)) yLimits = yRan
  if(missing(xLimits)) xLimits = xRan
  
  if(missing(yLabel)) yLabel = "Response"
  if(missing(xLabel)) xLabel = "Time (units)"
  
  ## create the correct formula
  ctp = vpc$covariates[-1]
  if(length(ctp) == 0) groupFunc = ""
  if(length(ctp)>0)
  {
    groupCovs = unlist(lapply(ctp[seq(min(length(ctp),2))], function(xxx) paste("format(",xxx,")", sep="")))
    groupFunc = paste("|", groupCovs, sep = " ")
  }
  
  ## work on the formula to ensure grouping levels are passed on properly
  if(!missing(formula)){
    myGroupCov = unPaste(deparse(getGroupsFormula(formula)[[2]]), sep = "[*]")
    myGroupCov = paste(sapply(myGroupCov, function(x) paste("{",x, "}", sep = "")), collapse = " * ")
    formula = as.formula(
      paste(deparse(getResponseFormula(formula)[[2]]), deparse(getCovariateFormula(formula)), " | ", myGroupCov, sep = ""))
  }
  
  ## make final formula to plot predictions
  formText = paste("Cbind(Central, Lower, Upper, LowerQ, UpperQ) ~", vpc$covariates[1], groupFunc)
  if(!missing(formula)) theForm = formula	## use the specified one
  else theForm = as.formula(formText)	## if no formula specified use the automatically created one
  
  ## get observations
  if(is.null(obsData)) obsData = getData(object)
  obsData$theResponse = eval(as.name(getResponseFormula(object)[[2]]), obsData)
  obsGroupForm = if(missing(formula)) deparse(getGroupsFormula(theForm)[[2]]) else (myGroupCov)
  obsForm = as.formula(paste("theResponse", deparse(getCovariateFormula(theForm)), "|", obsGroupForm))
  
  if(F) {
    y = Cbind(vpc$vpc.summary$Central, vpc$vpc.summary$Lower, vpc$vpc.summary$Upper, vpc$vpc.summary$LowerQ, vpc$vpc.summary$UpperQ)
    xyplot(theForm,
           data = vpc$vpc.summary,    
           scales = myScales,
           panel = function(x,y, ...){
             panel.xyplot(x,y, ..., type = "l")
             lpolygon(c(x, rev(x)), c(log10(attr(y, "other")[,4]), rev(log10(attr(y, "other")[,3]))), col = green[2])
             llines(x, log10(attr(y, "other")[,1]))
             llines(x, log10(attr(y, "other")[,2]))
           },
           method = "filled bands",
           yscale.components = myComponents,
           xlab = xLabel,
           ylab = yLabel,
           ylim = yLimits,
           xlim = xLimits)
  }
  
  plot.pred = 
    xyplot(theForm,
           data = vpc$vpc.summary,
           panel = panel.nlme.vpc.pred,
           OBS = obsData,
           vpc = vpc$vpc.summary,
           logY = logY,
           showPredAs = showPredAs,
           showObsDots = showObsDots,
           showObsLines = showObsLines,
           col.scheme = col.scheme,
           obscex.dot = obscex.dot, 
           obspch.dot = obspch.dot,
           between = list(x = 0.5, y = 0.5),
           scales = myScales,
           yscale.components = myComponents,
           xlab = xLabel,
           ylab = yLabel,
           ylim = yLimits,
           xlim = xLimits)	
  
  plot.obs = 
    xyplot(obsForm,
           data = obsData,
           panel = panel.nlme.vpc.obs,
           OBS = obsData,
           vpc = vpc$vpc.summary,
           logY = logY,
           showPredAs = showPredAs,
           showObsDots = showObsDots,
           showObsLines = showObsLines,
           col.scheme = col.scheme,
           obscex.dot = obscex.dot, 
           obspch.dot = obspch.dot,
           between = list(x = 0.5, y = 0.5),
           scales = myScales,
           yscale.components = myComponents,
           xlab = xLabel,
           ylab = yLabel,
           ylim = yLimits,
           xlim = xLimits)	
  
  
  print(plot.pred, split = c(1,1,1,1), more = TRUE)
  print(plot.obs,  split = c(1,1,1,1), more = FALSE)
}

