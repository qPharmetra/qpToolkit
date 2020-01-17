# name:     nlme.predict
# purpose:  predict residuals, partial residuals or uncertainty predictions of nlme.object
# input:    function defining prediction space of interest, and nlme object
# output:   list with observed and predicted data, either raw or summarized
# note:     has not been thoroughly tested for objects with extended random, weights, and/or correlation structures

## "debug" line: run this line to get objects loaded to debug the function if needed
#func = value ~ time  | dose; object =  fit.PD004.nlme$object; xrange = 50;level = 1; method = "partial.residuals";  uncertainty = F ; nrep = 20;   reference.subset = NULL

# ROXYGEN Documentation
#' Prediction tool for nlme object
#' @description Predict residuals, partial residuals or uncertainty predictions for an nlme.object. This function will also take different data set (argument \code{newdata}) to simulate out of the box although the nlme object does require the predictors, independent variable, and the grouping variable to be present.
#' @param func the function to evaluate over the model, e.g. \code{effect ~ time | treatment} the variables in \code{func} need to be evaluable in \code{getData(object)} and  \code{getCovariateFormula(object)}
#' @param object nlme object
#' @param newdata a new data set, requiring the predictors, independent variable, and the grouping variable to be present.
#' @param subset.modeldata character string to be evaluated over the data in order to subset the predictions and observations (e.g. "DOSE == 100")
#' @param xrange number of equal-space independent varaible (x) default value = 50,
#' @param nx number of bins of independenet ,
#' @param level grouping level of the nlme object to create predictions at (defaults to 1, the highest grouping level)
#' @param method Either one of 'partial.residuals' (default), 'prediction' (the traditional \code{predict(nlme.object)}), or 'residuals' (doing \code{plot(resid ~ any.column)}.
#' @param use.etas 'estimated' (default) which uses the estimated deviations for each group level) or 'sampled' in case one wants to sample using the estimate of random variability
#' @param uncertainty logical (defaults to F) determining if simulation should be done across uncertainty
#' @param reference.subset character string to be evaluated over the data in order to subset the predictions and observations in ordeer to create a 'change from' perspective plot (works for method 'prediction' and 'partial residuals')
#' @param nrep number of replicates to simulate. Defaults to 20.
#' @return a graph or a multi-level list with observed and predicted output (class \code{c('nlmefit','fit','list')})
#' @export nlme.predict
#' @importFrom Hmisc summarize
#' @importFrom Hmisc smean.cl.normal
#' @importFrom MASS mvrnorm
#' @importFrom nlme getData
#' @importFrom nlme getCovariateFormula
#' @importFrom nlme fixef
#' @importFrom nlme ranef
#' @importFrom nlme getResponseFormula
#' @importFrom nlme nlme
#' @importFrom nlme getGroups
#' @importFrom nlme getGroupsFormula
#' @importFrom nlme random.effects
#' @examples
#' pkpdData = example.pkpdData()
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
#' fit.PD004.nlme = nlme.run(
#'   model = value ~ base + EFF.1comp.1abs(dose, time, cl * exp(cl.eta), v, ka, keo),
#'   data = pkpdData[pkpdData$type == "PD" & pkpdData$dose > 0 & pkpdData$value > 0.5, ],
#'   fixed = base + cl + v + ka + keo ~ 1,
#'   random = cl.eta ~ 1,
#'   groups = ~ id,
#'   start = c(base = 1, cl = 1, v = 10, ka = 1, keo = 0.01),
#'   problem = "True Model",
#'   reference = 4)
#' summary(fit.PD004.nlme$object)
#' nlme.extract(fit.PD004.nlme$object)$table
#'
#' # simple fit vs time
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time , fit.PD004.nlme$object)
#' plot(fit.PD004.pred.nlme)
#' fit.PD004.pred.nlme = nlme.predict(
#'   func = value ~ time , fit.PD004.nlme$object, method = "partial.residuals"
#' )
#' plot(fit.PD004.pred.nlme) ## this is the same: PARTIAL RESIDUALS
#' fit.PD004.pred.nlme = nlme.predict(
#'   func = value ~ time , fit.PD004.nlme$object, method = "prediction"
#' )
#' plot(fit.PD004.pred.nlme) ## now we get simply the prediction for all xCovariate
#'
#' ## note that prediction and partial residual type of model fit plots are very different
#'
#' # fit vs time by dose
#' fit.PD004.pred.nlme = nlme.predict(func = value ~ time | dose, fit.PD004.nlme$object)
#' plot(fit.PD004.pred.nlme)
#' fit.PD004.pred.nlme = nlme.predict(
#'   func = value ~ time | dose, fit.PD004.nlme$object, method = "residuals"
#' )
#' plot(fit.PD004.pred.nlme, yLimits = c(-1,1))
#' plot(fit.PD004.pred.nlme, yLimits = c(-1,1), abline = list(h = 0, lty = 2))

nlme.predict <- function(
  func,
  object,
  newdata = NULL,
  subset.modeldata,
  xrange = 50,
  nx = NULL,
  level = 1,
  method = "partial.residuals",
  use.etas = "estimated",
  uncertainty = FALSE,
  reference.subset = NULL,
  nrep = 20)
{
  if(method %nin% c('partial.residuals', 'prediction', 'residuals')){
    message(" -- argument 'method' needs to be 'partial.residuals', 'prediction', or 'residuals'"); return()
  }
  if(use.etas %nin% c('estimated', 'sampled', 'fixed.to.zero')){   # 9/24/2011 FLH
    message(" -- argument 'etas' needs to be 'estimated', 'fixed.to.zero', or 'sampled'"); return()
  }
  obsData = getData(object)

  ## deal with subsetting
  filter.seq = rep(TRUE, nrow(obsData))
	if(!missing(subset.modeldata))
  {
    message(" -- observation data subsetted by ", subset.modeldata)
    olddata = obsData
    filter.seq = eval(parse(text = subset.modeldata), obsData)
    obsData = obsData[filter.seq, ]
  }

  if(is.null(newdata)){
    newdata = obsData
  }

  ## parse out the plotting function
  xfLevel = getCovariateFormula(func)[[2]]
  xfVarNames = all.vars(xfLevel)
  yfLevel = getResponseFormula(func)[[2]]
  yfVarNames = all.vars(yfLevel)
  gfLevel = getGroupsFormula(func)[[2]]
  gfVarNames = all.vars(gfLevel)
  gpLevel = getGroupsFormula(object)[[2]]
  gpNames = all.vars(gpLevel)
  rmLevel = getResponseFormula(object)[[2]]
  rmNames = all.vars(rmLevel)

  ## binning by X if requested
  if(!is.null(nx) & is.numeric(nx))
  {
    if(length(nx) == 1) ## cut3(x, g = nx)
    {
      newdata[, xfVarNames] = cut3(as.numeric(newdata[, xfVarNames]), g = nx, levels.mean = TRUE)
      obsData[, xfVarNames] = cut3(as.numeric(obsData[, xfVarNames]), g = nx, levels.mean = TRUE)
    } else {
      newdata[, xfVarNames] = cut3(as.numeric(newdata[, xfVarNames]), cuts = nx, levels.mean = TRUE)
      obsData[, xfVarNames] = cut3(as.numeric(obsData[, xfVarNames]), cuts = nx, levels.mean = TRUE)
    }
  }

  ## check for 1 grouping level
  if(length(gpNames) > 1) {message(" -- WARNING --- predictNLME supports one grouping level only!")}

  objectForm = getCovariateFormula(object)[[2]]
  allX = all.vars(objectForm)
  allX.func = all.vars(getCovariateFormula(func)[[2]])

  objPars = stats::coef(object)
  fixpars = fixef(object)
  uncPars = data.frame(mvrnorm(nrep, fixpars, object$varFix))
  fixPars = data.frame(matrix(fixpars, nrow = 1))
  names(fixPars) = names(uncPars) = names(fixpars)

  covFormVars = unique(c(allX[allX %nin% names(objPars)],  gfVarNames, allX.func))
    ## from the covariate formula
  keyPredictionVariables = c(covFormVars, gpNames)
    ## from the covariate formula + grouping level

  if(any(rmNames %nin% keyPredictionVariables))
  {
    for(i in lunique(rmNames[rmNames %nin% keyPredictionVariables]))
    {
      newdata$dummy = rep(1,nrow(newdata))
      names(newdata)[names(newdata) == "dummy"] =
        rmNames[rmNames %nin% keyPredictionVariables][i]
    }
    keyPredictionVariables = c(keyPredictionVariables,
      rmNames[rmNames %nin% keyPredictionVariables])
  }
    ## in case of multiple model response variables

  ## check presence of key prediction variables and stop gracefully with infomrion if any is missing
  if(any(covFormVars %nin% names(newdata)))
  {
    message(" -- columns (", paste(covFormVars, collapse = ", "), ") required but argument 'newdata' features (",
      paste(names(newdata), collapse = ", "), ")")
      message(" -- predictNLME procedure stopped.")
    return()
  }

  ## check for presence of key variables, populate with 'first' individual if grouping level does not exist
  if(!any(gpNames %in% names(newdata)))
  {
    newdata$theGroup = rep(unique(getGroups(object))[1], times = nrow(newdata))
    names(newdata)[names(newdata) == "theGroup"] = gpNames
    use.etas = "fixed.to.zero"
    level = 0
  }

  ## select data set
  mpData = as.data.frame(newdata[, keyPredictionVariables])
  names(mpData) = keyPredictionVariables

  ## create data frame for smooth predictions
  if(method == "partial.residuals")
  {
    x = eval(xfLevel, mpData)
    xval = sunique(as.vector(x, "numeric"))
    xval = seq(min(xval), max(xval),length = xrange)
    xnew = expand.grid(xval)
    newdatanames = names(mpData)
    rpnum = nrow(xnew)
    idx = rep(1 : rpnum, nrow(mpData))
    xnew = data.frame(xnew[idx,])
    idx = matrix(rep(1:nrow(mpData), rpnum), ncol = nrow(mpData), nrow = rpnum, byrow = TRUE)
    idx = matrix(idx, ncol = 1, byrow = TRUE)
    simData = as.data.frame(mpData[idx, ])
    simData[, names(simData) %in% xfVarNames] = c(xnew)
    #names(simData)
  } else simData = mpData

  ## merge group levels in
  dim(simData)
  etas = random.effects(object)
  eta.names = names(etas)
  if(level == 0){
   etas = data.frame(matrix(0, ncol = ncol(etas), nrow = nrow(etas)))
   names(etas) = eta.names
  }
  etas$theGroup = row.names(etas)
  names(etas)[names(etas) == "theGroup"] = gpNames

  simData = merge(simData, etas, by = gpNames, all.x = TRUE)
  dim(simData)
  #str(simData)

  ## merge parameters in
  if(method == "partial.residuals" | uncertainty == FALSE){
    parData = fixPars[rep(1, nrow(simData)), ]
    simData = cbind(simData, parData)
    simData$ypr = eval(objectForm, simData)
    if(!is.null(reference.subset)){
      newText = paste("simData[simData$", reference.subset, ", ]", sep = "")
      refData = eval(parse(text = newText))
      refData = refData[rep(1, nrow(simData)), ]
      refData$ypr = stats::predict(object, refData)
      simData$ypr = simData$ypr - refData$ypr
    }
  }

  ## predicted values for new simulation data
  if(method != "partial.residuals" | uncertainty == TRUE){
    simData$ypr = stats::predict(object, newdata, level = level)
    if(!is.null(reference.subset)){
      newText = paste("newdata[newdata$", reference.subset, ", ]", sep = "")
      refData = eval(parse(text = newText))
      refData = refData[rep(1, nrow(newdata)), ]
      refData$ypr = stats::predict(object, refData)
      simData$ypr = simData$ypr - refData$ypr
    }
  }

  uncPred = NULL

  if(method != "partial.residuals" & uncertainty == TRUE){
  uncPred = sapply(1:nrep, function(x, fxpars, data, form, nms.fix, theETAS, nms.eta, reference.subset, group.var)  #FLH 9/24/2011
    {
      #sampled.etas = apply(as.matrix(theETAS), 2, function(y, ids) sample.by.id(ids, y), ids = mpData$id)
      sampled.etas = apply(as.matrix(theETAS), 2, function(y, ids) sample.by.id(ids, y), ids = data[,group.var])  # FLH 9/24/2011
      sData = cbind(data, data.frame(matrix(rep(unlist(fxpars[x, ]), nrow(data)), ncol = length(fxpars[x,]), byrow = TRUE)), data.frame(sampled.etas))
      names(sData) = c(names(data), nms.fix, nms.eta)  ; head(sData)
      ypr = eval(form, sData)
      if(!is.null(reference.subset))
      {
        newText = paste("sData[sData$", reference.subset, ", ]", sep = "")
        refData = eval(parse(text = newText))
        refData = refData[1, ]
        refData = refData[rep(1, nrow(sData)), ]
        ypr.reference = eval(form, refData)
        ypr = ypr - ypr.reference
      }
      return(ypr)
    },  form = objectForm,
        data = mpData,
        fxpars = uncPars,
        nms.fix = names(fixpars),
        theETAS = etas[, names(etas) %nin% gpNames],
        nms.eta = names(etas)[names(etas) %nin% gpNames],
        reference.subset = reference.subset,
        group.var = gpNames)              # FLH 9/24/2011
  } ## end if method

  ## summarization lists
  create.list <- function(data, nams)
  {
    myList = list(data[, nams])
    if(length(nams)>1) myList = as.list(data[, nams])
    names(myList) = nams
    return(myList)
  }
  sList.sim = if(length(gfVarNames) == 0) create.list(simData, xfVarNames) else create.list(simData, c(xfVarNames, gfVarNames))
  sList.obs = if(length(gfVarNames) == 0) create.list(obsData, xfVarNames) else create.list(obsData, c(xfVarNames, gfVarNames))

  ## summarize predicted and observed on the model.dataset
  qypr = Hmisc::summarize(simData$ypr,            sList.sim, smean.cl.normal, stat.name = "Mean")
  qobs = Hmisc::summarize(eval(yfLevel, obsData), sList.obs, smean.cl.normal, stat.name = "Mean")

  xObs = eval(xfLevel, obsData)
  theResiduals = stats::resid(object)[filter.seq]
  yObs = eval(yfLevel, obsData)
  yPrd = stats::predict(object, level = level)[filter.seq]
   if(!is.null(reference.subset)){
      newText = paste("obsData[obsData$", reference.subset, ", ]", sep = "")
      refData = eval(parse(text = newText))
      refData = refData[rep(1, nrow(obsData)), ]
      refData$ypr = stats::predict(object, refData)
      yPrd = yPrd - refData$ypr
    }
  qYPred = Hmisc::summarize(yPrd, sList.obs, mean) # predicted mean predict(model.dataset) by X | Z

  ## create partial residuals
  ##  approximate the predicted relationship for simulation data
  ##  by adding residuals to arrive at partial residuals (RES = DV+PRED)
  obsData$partres = rep(NA, nrow(obsData))
  qpar = NULL

  if(method == "partial.residuals")
  {
  if(length(gfVarNames) == 0){
    obsData$partres = stats::approx(qYPred[, xfVarNames], qYPred$yPrd, xObs)$y + theResiduals
    } else {
      gf.levels = sunique(eval(gfLevel,qYPred))
      for(i in 1 : length(gf.levels)){
        msel.pred = qYPred[, gfVarNames] == gf.levels[i]
        msel.obs = obsData[,gfVarNames] == gf.levels[i]
        obsData$partres[msel.obs] = stats::approx(qYPred[msel.pred, xfVarNames], qYPred$yPrd[msel.pred], xout = xObs[msel.obs])$y + theResiduals[msel.obs]
        }
    } ## end else {

  ## summarize partial residuals
  qpar = Hmisc::summarize(obsData$partres, sList.obs, smean.cl.normal, stat.name = "Mean")
  } ## end if(method

  if(method == "residuals")
  {
    obsData$partres = stats::resid(object)
    qpar = Hmisc::summarize(obsData$partres, sList.obs, smean.cl.normal, stat.name = "Mean")
    qypr$Mean = qpar$Mean
  }

  if(method == "prediction")
  {
    obsData$partres = eval(yfLevel, obsData)
    qpar = Hmisc::summarize(obsData$partres, sList.obs, smean.cl.normal, stat.name = "Mean")
  }

  ## return resulting data frame
   out <- list(  object.name = deparse(substitute(object)),
                level = level,
                func = func,
                method = method,
                object = object,
                obsData = obsData, ## contains observed data AND partial residuals
                pred = list(obs = qobs, pred = qypr, partres = qpar, uncPred = uncPred)
              )
  class(out) <- c('nlmefit','fit','list')
  out
}

