
# ROXYGEN Documentation
#' Plotting function for nls.predict and nlme.predict
#' @description Plots residual, predicted, or partial residial plots based on x generated by qP functions nlme.predict and nls.predict.
#' @param x the output from \code{nls.predict} or \code{nlme.predict}, i.e. class \code{'fit'}
#' @param ... any other arguments to be passed on to the lattice call
#' @param newFunc the function to plot. If not suppliued will be taken from \code{x}
#' @param yLabel same as \code{ylab}
#' @param xLabel same as \code{xlab}
#' @param label.cex fontmszie of axes labels
#' @param xLimits same as \code{xlim}
#' @param yLimits same as \code{ylim}
#' @param title place a title above
#' @param plot.title logical indicating if a plot title should be written
#' @param layout will be passed on a lattice argument \code{layout}
#' @param aspect lattice banking aspect
#' @param mnplot logical indicating an average plot to be created as opposed to 'raw data
#' @param seplot logical indicating is error bars need to be drawn
#' @param logX logarithmic X axis?
#' @param logY logarithmic Y axis?
#' @param pointcol color of dots
#' @param linecol color of
#' @param axis.lim.widener scalar to stretch the x and y axis. Use in case data or predictions are not visible anymore
#' @param do.plot Defaults to T (create the plot) if F it will output the prediction data
#' @param abline will be passed on to the lattice call as is
#' @return A plot
#' @export
#' @import lattice
#' @importFrom stats as.formula
#' @importFrom nlme getResponseFormula
#' @importFrom nlme getCovariateFormula
#' @importFrom nlme getGroupsFormula
#' @examples
#' pkpdData = example.pkpdData()
#'
#' DNase1 <- subset(DNase, Run == 1)
#'
#' ## using a selfStart model
#' fm1DNase1 <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
#' summary(fm1DNase1)
#'
#' fm1DNase1.predict = nls.predict(density ~ conc,object = fm1DNase1)
#' plot(fm1DNase1.predict)
#' fm1DNase1.predict = nls.predict(density ~ conc,object = fm1DNase1)
#' plot(fm1DNase1.predict, logX = TRUE)
#'
#' EFF.1comp.1abs <- function(dose, tob, cl, v, ka, keo)
#' {
#'   # Effect-site concentration for 1-compartment model, 1st-order absorption
#'
#'   kel = cl / v
#'
#'   # Define coefficients
#'   A = 1/(kel-ka) / (keo-ka)
#'   B = 1/(ka-kel) / (keo-kel)
#'   C = 1/(ka-keo) / (kel-keo)
#'
#'   # Return effect-site concentration
#'   dose*ka*keo/v * (A*exp(-ka*tob) + B*exp(-kel*tob) + C*exp(-keo*tob))
#' }
#' fit.PD004.nlme = nlme.run(model = value ~ base +
#'  EFF.1comp.1abs(dose, time, cl * exp(cl.eta), v, ka, keo)
#'  , data = subset(pkpdData,type == "PD" & dose > 0 & value > 0.5),
#'                           fixed = base + cl + v + ka + keo ~ 1,
#'                           random = cl.eta ~ 1,
#'                           groups = ~ id,
#'                           start = c(base = 1, cl = 1, v = 10
#'                           , ka = 1, keo = 0.01),
#'                           problem = "True Model",
#'                           reference = 4)
#' summary(fit.PD004.nlme$object)
#' nlme.extract(fit.PD004.nlme$object)$table
#'
#' # simple fit vs time
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time
#'    , fit.PD004.nlme$object)
#' plot(fit.PD004.pred.nlme)
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time
#'    , fit.PD004.nlme$object, method = "partial.residuals")
#' plot(fit.PD004.pred.nlme) ## this is the same: PARTIAL RESIDUALS
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time
#'    , fit.PD004.nlme$object, method = "prediction")
#' plot(fit.PD004.pred.nlme) ## 'simple' prediction for all xCovariate
#'
#' ## note that prediction and partial residual type of model fit
#' ## plots are very different
#'
#' # fit vs time by dose
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time | dose
#'    , fit.PD004.nlme$object)
#' plot(fit.PD004.pred.nlme)
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time | dose
#'    , fit.PD004.nlme$object, method = "residuals")
#' plot(fit.PD004.pred.nlme, yLimits = c(-1,1))
#' plot(fit.PD004.pred.nlme, yLimits = c(-1,1)
#'    , abline = list(h = 0, lty = 2))
plot.fit <- function(x, ..., newFunc,
  yLabel, xLabel, label.cex = 1.25,
  xLimits, yLimits,
  title, plot.title = TRUE,
  layout, aspect,
  mnplot = TRUE, seplot = TRUE,
  logX = FALSE, logY = FALSE,
  pointcol = gray[8],
  linecol = qp.blue,
  axis.lim.widener = 0.035,
  do.plot = TRUE ,
  abline = NULL)
{
  func = if(missing(newFunc)) x$func else newFunc
  yfLevel = nlme::getResponseFormula(func)[[2]]
  yfVarNames = all.vars(yfLevel)
  xfLevel = nlme::getCovariateFormula(func)[[2]]
  xfVarNames = all.vars(xfLevel)
  gfLevel = nlme::getGroupsFormula(func)[[2]]
  gfVarNames = all.vars(gfLevel)

  plotForm = paste("Cbind(Mean, Lower, Upper) ~", xfVarNames)
  if(length(gfVarNames)>0) plotForm = paste(plotForm, "|", deparse(gfLevel))
  plotForm = as.formula(plotForm)

  plotData = x$pred$pred

  ## labels
  if(missing(yLabel)) yLabel = x$method
  if(missing(xLabel)) xLabel = paste(xfLevel)

  ## axes limits
  if(missing(yLimits)){
   if(mnplot){
    yLimits = range(c(unlist(x$pred$pred[, c('Lower', 'Upper')]),unlist(x$pred$partres[, c('Lower', 'Upper')])), na.rm = TRUE)
    yLimits = yLimits + c(-1.02, 1.02) * diff(yLimits) * axis.lim.widener
    }
   if(!mnplot) yLimits = range(c(x$obsData$partres,x$pred$pred$Mean), na.rm = TRUE)
  }
  if(missing(xLimits)) {
    xLimits = range(x$pred$pred[, xfVarNames], na.rm = TRUE)
    xLimits = xLimits + c(-1.02, 1.02) * diff(xLimits) * axis.lim.widener
    }
  if(x$method == "residuals" & mnplot) yLimits = range(c(x$pred$partres[,c('Lower','Upper')]), na.rm = TRUE)
  if(x$method == "residuals" & !mnplot) yLimits = range(c(x$obsData$partres), na.rm = TRUE)

  ## scales
  myXscale = myYscale = list(log = FALSE, cex = label.cex)
  if(logY) myYscale = list(log = 10, cex = label.cex)
  if(logX) myXscale = list(log = 10, cex = label.cex)
  myScales = list(x = myXscale, y = myYscale)

  myXcomp = xscale.components.default
  myYcomp = yscale.components.default
  if(logY) myYcomp = yscale.components.log10
  if(logX) myXcomp = xscale.components.log10

  ## title
  myTitle = if(missing(title))
    paste("predict(", x$object.name,", level = ", if(is.null(x$level)) 1 else x$level, ")", sep = "") else title

  thePlot = xyplot(plotForm,
    data = x$pred$pred,
    partres = x$pred$partres,
    subscripts = TRUE,
    main = myTitle,
    yfv = yfVarNames,
    lc = linecol,
    pc = pointcol,
    xfv = xfVarNames,
    gfv = gfVarNames,
    xlim = xLimits,
    ylim = yLimits,
    xscale.components = myXcomp,
    yscale.components = myYcomp,
    scales = myScales,
    mnplot = mnplot,
    abline = abline,
    seplot = seplot,
    ylab = list(yLabel, cex = label.cex),
    xlab = list(xLabel, cex = label.cex),
    pData = x$pred$pred,
    obsData = x$obsData,
    lgx = logX,
    lgy = logY,
    panel = function(x, y, subscripts, ..., partres, yfv, xfv, gfv, obsData, pData, lgy, lgx, lc, pc, mnplot, seplot)
      #subscripts = pData$dose == 10
    {
      panel.xyplot(x, y,  ..., type = "l", col = lc, lwd = 3)

      if(mnplot)
      {
        xpar = partres[, xfv]
        ypar = partres$Mean
        yup = partres$Upper
        ylo = partres$Lower
        if(!(inherits(partres[, gfv], "data.frame")) & length(gfv)>0)
        {
           origSubs = unique(pData[, gfv][subscripts])
           mySubs = partres[, gfv] == origSubs
           xpar = xpar[mySubs]
           ypar = ypar[mySubs]
           yup  = yup[mySubs]
           ylo  = ylo[mySubs]
        }
        xpar = transform_log10(xpar, log = lgx)
        ypar = transform_log10(ypar, log = lgy)
        ylo  = transform_log10(ylo, log = lgy)
        yup  = transform_log10(yup, log = lgy)

      ## partial residuals
      lpoints(xpar, ypar, cex = 2, col = pc)

      ## CI bars
      if(seplot)
      {
        lsegments(x0 = xpar, x1 = xpar, y0 = ylo, y1 = yup, col = gray[8])

        ## CI bar ends
        wi = rep(range(x, na.rm = TRUE) / 100, length(xpar))
        lsegments(x0 = xpar-wi, x1 = xpar+wi, y0 = ylo, y1 = ylo, col = gray[8])
        lsegments(x0 = xpar-wi, x1 = xpar+wi, y0 = yup, y1 = yup, col = gray[8])
      } # end  ! all(
      } # end if(mnplot)

      if(!mnplot)
      {
         origSubs = unique(pData[, gfv][subscripts])
         mySubs = obsData[, gfv] == origSubs
         obsData[mySubs, "partres"] = transform_log10(obsData[mySubs, "partres"], lgx)
         obsData$partres[mySubs] = transform_log10(obsData$partres[mySubs], lgy)
         lpoints(obsData[mySubs, "partres"], obsData$partres[mySubs], cex = 1, col = gray[8])
      } # end if(!mnplot)
      if(!is.null(abline)) do.call("panel.abline", abline)
    } # end panel

    )
    if(!missing(layout)) thePlot$layout = layout
    if(!missing(aspect)) thePlot$aspect = aspect
    if(do.plot) print(thePlot) else return(thePlot)
}

if(F)
{
  EFF.1comp.1abs <- function(dose, tob, cl, v, ka, keo)
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

  # simple fit vs time
  fit.PD004.pred.nlme = nlme.predict(func = value ~ time , fit.PD004.nlme$object)
  plot(fit.PD004.pred.nlme)
  fit.PD004.pred.nlme = nlme.predict(func = value ~ time , fit.PD004.nlme$object, method = "partial.residuals")
  plot(fit.PD004.pred.nlme) ## this is the same: PARTIAL RESIDUALS
  fit.PD004.pred.nlme = nlme.predict(func = value ~ time , fit.PD004.nlme$object, method = "prediction")
  plot(fit.PD004.pred.nlme) ## now we get simply the prediction for all xCovariate

  ## note that prediction and partial residual type of model fit plots are very different

  # fit vs time by dose
  fit.PD004.pred.nlme = nlme.predict(func = value ~ time | dose, fit.PD004.nlme$object)
  plot(fit.PD004.pred.nlme)
  fit.PD004.pred.nlme = nlme.predict(func = value ~ time | dose, fit.PD004.nlme$object, method = "residuals")
  plot(fit.PD004.pred.nlme, yLimits = c(-1,1))
  plot(fit.PD004.pred.nlme, yLimits = c(-1,1), abline = list(h = 0, lty = 2))

}
