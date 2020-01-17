# name:     nls.predict
# purpose:  predict partial residuals  of an nls object
# input:    funcion to create and summarize partial residuals across and the nls object
# output:   list with object, partial residuals and predicted output
# note:     plot.fit is the associated plotting routine for the output of this function

# ROXYGEN Documentation
#' Predict partial residuals from an nls object
#' @description Predict partial residuals from an nls object to be plotted with \code{plot.fit}
#' @param func formula of \code{y ~ x | z} detailing what to predict. Requires \code{y} to be the response variable of the nls obhect
#' @param object and nls object
#' @param newdata optional dataset to simulate across. Needs the elements required to create the prediction
#' @param xrange number of equal-spaced x covariate values created from \code{min(x)} to \code{max(x)}
#' @return a graph or a multi-level list with observed and predicted output (class \code{c('nlsfit','fit','list')})
#' @export
#' @importFrom Hmisc summarize smean.cl.normal
#' @importFrom nlme getData getCovariateFormula fixef ranef
#' @importFrom nlme getResponseFormula nlme
#' @importFrom nlme getGroupsFormula
#' @importFrom stats coef predict resid approx
#' @seealso \code{\link{plot.fit}}, \code{\link{nlme.predict}}
#' @examples
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

nls.predict <- function(func,
  object,
  newdata=NULL,
  xrange = 150)
{
  obsData = getData(object)
  if(is.null(newdata))
    newdata = obsData

  ## parse out the plotting function
  xfLevel = getCovariateFormula(func)[[2]]
  xfVarNames = all.vars(xfLevel)
  yfLevel = getResponseFormula(func)[[2]]
  yfVarNames = all.vars(yfLevel)
  gfLevel = getGroupsFormula(func)[[2]]
  gfVarNames = all.vars(gfLevel)

  objectForm = getCovariateFormula(object)[[2]]
  allX = all.vars(objectForm)

  objPars = coef(object)

  keyPredictionVariables = allX[allX %nin% names(objPars)]
  mpData = as.data.frame(newdata[, keyPredictionVariables])
  names(mpData) = keyPredictionVariables

  ## create data frame for predictions
  x = eval(xfLevel, mpData)
  xval = sunique(x)
  xval = seq(min(xval), max(xval),length = xrange)
  xnew = expand.grid(xval)
  newdatanames = names(newdata)
  rpnum = nrow(xnew)
  idx = rep(1 : rpnum, nrow(newdata))
  xnew = data.frame(xnew[idx,])
  idx = matrix(rep(1:nrow(newdata), rpnum), ncol = nrow(newdata), nrow = rpnum, byrow=TRUE)
  idx = matrix(idx, ncol=1, byrow=TRUE)
  newdata = as.data.frame(newdata[idx, ])
  newdata[, names(newdata) %in% xfVarNames] = c(xnew)
  names(newdata)

  ## predictions
  newdata$ypr = predict(object, newdata)

  ## summarization lists
  create.list <- function(data, nams)
  {
    myList = list(data[, nams])
    if(length(nams)>1) myList = as.list(data[, nams])
    names(myList) = nams
    return(myList)
  }
  sList.sim = if(length(gfVarNames)==0) create.list(newdata, xfVarNames) else create.list(newdata, c(xfVarNames, gfVarNames))
  sList.obs = if(length(gfVarNames)==0) create.list(obsData, xfVarNames) else create.list(obsData, c(xfVarNames, gfVarNames))

  ## summarize predicted and observed on the model.dataset
  qypr = Hmisc::summarize(newdata$ypr,     sList.sim, smean.cl.normal, stat.name = "Mean")
  qobs = Hmisc::summarize(eval(yfLevel, obsData),     sList.obs, smean.cl.normal, stat.name = "Mean")

  xObs = eval(xfLevel, obsData)
  theResiduals = resid(object)
  yObs = eval(yfLevel, obsData)
  yPrd = predict(object)
  qYPred = Hmisc::summarize(yPrd, sList.obs, mean) # predicted mean predict(model.dataset) by X | Z

  ## create partial residuals
  obsData$partres = rep(NA, nrow(obsData))
  qpar = NULL

  ## uncertainty predictions??
  ## to be merged in from predictNLS2
  uncPred = NULL

  if(length(gfVarNames)==0){
    obsData$partres = approx(qYPred[, xfVarNames], qYPred$yPrd, xObs)$y + theResiduals
    } else {
      gf.levels = sunique(eval(gfLevel,qYPred))
      for(i in 1 : length(gf.levels)){
        msel.pred = qYPred[, gfVarNames] == gf.levels[i]
        msel.obs = obsData[,gfVarNames] == gf.levels[i]
        obsData$partres[msel.obs] = approx(qYPred[msel.pred, xfVarNames], qYPred$yPrd[msel.pred], xout = xObs[msel.obs])$y + theResiduals[msel.obs]
        }
    } ## end else {

  ## summarize partial residuals
  qpar = Hmisc::summarize(obsData$partres, sList.obs, smean.cl.normal, stat.name = "Mean")

  out <- list(  object.name = deparse(substitute(object)),
                level = 0,
                method = "partial.residuals",
                func = func,
                object = object,
                obsData = obsData, ## contains observed data AND partial residuals
                pred = list(obs = qobs, pred = qypr, partres = qpar, uncPred = uncPred)
              )
  class(out) <- c('nlsfit','fit', class(out))
  out
}
