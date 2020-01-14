# name: nlme.diag
# purpose: given an R nlme model object, create two diagnostic plots of the eta(s) in the model
# input: an R nlme model object, and various arguments to tailor the graphic.
# output: a standard normal Q-Q plot of the eta estimates, and a graphic showing the distribution of
#       etas by binned values of an independent variable.

# ROXYGEN Documentation
#' Diagnostic plots for nlme
#' @description Given an R nlme model object, create two diagnostic plots of the eta(s) in the model
#' @param obj nlme model object
#' @param subset.modeldata character string to subset the data
#' @param xvar the column name of the independent variable
#' @param xvar.label label for independent variable, defaulting to NULL (no label)
#' @param nx number of bins of \code{xvar} to bin across
#' @param output logical determining if the plotting data should be outputted
#' @param print.eta.norm logical determining if a standard normal Q-Q plot of the eta estimates will be plotted print.eta.v.var
#' @param print.eta.v.var logical determining if a distribution of etas by binned values of \code{xvar} will be plotted
#' @param asp.eta.norm aspect value for the QQ plot
#' @param asp.eta.v.var aspect vlaue of the plot for eta vs variables
#' @return Diagnostic plots
#' @export nlme.diag
#' @seealso \code{\link{nlme.run}}, \code{\link{nlme.predict}}
#' @importFrom nlme getData getCovariateFormula fixef ranef
#' @importFrom nlme getResponseFormula nlme
#' @importFrom nlme getGroupsFormula
#' @import lattice
#' @importFrom reshape2 melt
#' @importFrom stats residuals fitted predict as.formula median
#' @importFrom Hmisc panel.bpplot
#' @examples
#' ## define modeling function
#' ## adapted from pk.1comp.1abs to make it modeling-ready
#' library(nlme)
#' pkpdData = example.pkpdData()
#' PK.1comp.1abs =
#' function(dose, tob, cl, v, ka){
#'   kel = cl / v
#'   dose * ka/v/(ka-kel) * (exp(-kel*tob) - exp(-ka*tob))
#' }
#'
#' ## fit 1 comp PK with 1st order absorption
#' fit.nlme.1 = nlme.run(value ~ PK.1comp.1abs(dose, time, cl*exp(cl.eta), v*exp(v.eta), ka),
#'                      data = subset(pkpdData, type == "PK" & dose> 0 & value > 0.1),
#'                      groups = ~ id,
#'                      fixed = cl + v + ka ~ 1,
#'                      random = pdDiag(list(cl.eta~1,v.eta ~ 1)),
#'                      start = c(cl = 1, v = 5, ka = 1),
#'                      reference = 3,
#'                      problem = "1comp.1abs eta(CL)"
#' )
#'
#' summary(fit.nlme.1$object)
#' nlme.diag(fit.nlme.1$object)
#' # note here we refer to the $object, given the model was created with nlme.run()
nlme.diag = function(obj, subset.modeldata, xvar="time", xvar.label=NULL,
  nx=8, output = FALSE, print.eta.norm=TRUE, print.eta.v.var=TRUE,
  asp.eta.norm=1, asp.eta.v.var=1){

  if (is.null(xvar.label)) xvar.label = xvar

  # Make model diagnostic plots
  ds = nlme::getData(obj)

  keyCovVars = all.vars(nlme::getCovariateFormula(obj)[[2]])
  keyCovVars = keyCovVars[!is.element(keyCovVars, c(names(nlme::fixef(obj)), names(nlme::ranef(obj))))]
#  if(length(keyCovVars)==0) { plot(ranef(obj)); return()}

  ## deal with subsetting
  filter.seq = rep(TRUE, nrow(ds))
	if(!missing(subset.modeldata))
  {
    message(" -- observation data subsetted by ", subset.modeldata)
    olddata = ds
    filter.seq = eval(parse(text = subset.modeldata), ds)
    ds = ds[filter.seq, ]
  }

  ds$resp = eval(nlme::getResponseFormula(obj)[[2]], ds)
  ds$resid = residuals(obj)[filter.seq]
  ds$fitted = fitted(obj)[filter.seq]
  ds$ipred = if(length(keyCovVars)==0) fitted(obj) else predict(obj, ds)
  ds$pred = if(length(keyCovVars)==0) fitted(obj, level = 0) else predict(obj, ds, level = 0)

  objectGrpName = all.vars(nlme::getGroupsFormula(obj)[[2]])
  if(length(objectGrpName)>1)
  {
    message(" -- only one grouping level supported. nlme.diag procedure stopped"); return()
  }
  unid = unique(ds[, objectGrpName])

  eta = nlme::ranef(obj)
  etaNames = names(eta)
  eta$id = as.vector(row.names(eta), class(unid))
  names(eta)[names(eta) == "id"] = objectGrpName
  eta = eta[is.element(eta[, objectGrpName], unid), ]
  ds = merge(ds, eta, by = objectGrpName)

  ## are etas normally distributed?
  theETAs = if(length(etaNames)==1) eta else reshape2::melt(data.frame(eta[, etaNames]))
  names(theETAs) = if(length(etaNames)==1) c('value','variable') else c('variable', 'value')
  if(length(etaNames)==1) theETAs$variable = rep(etaNames, nrow(theETAs))
  qqplot = qqmath( ~ value | variable, theETAs,
   panel = function(x, ...)
    {
        panel.qqmath(x, ..., col = gray[8], cex = 1)
        panel.qqmathline(x, ..., col = blue[8], lwd = 1.5)
    },
    ylab = list("Eta Estimate", cex = 1.25),
    xlab = list("Standard Normal Quantiles", cex = 1.25),
    aspect = asp.eta.norm
  )

   keyCovVars = all.vars(getCovariateFormula(obj)[[2]])
   keyCovVars = keyCovVars[!is.element(keyCovVars, c(names(fixef(obj)), etaNames))]

   if(length(keyCovVars)==0) {graphics::plot(qqplot); return()}
   if(missing(xvar)) xvar = keyCovVars[1]

   ## plot eta versus covariate specified in argument xvar

      etaDS = reshape2::melt(ds, measure.vars = etaNames, id.vars = xvar)
      theForm = as.formula(paste("format(",xvar,")", "~ value | variable"))
      if(lunique(etaDS[, xvar])>nx & is.numeric(etaDS[, xvar]))
      {
        etaDS$bins = cut3(etaDS[, xvar], g = nx)
        theForm = as.formula("format(bins) ~ value | variable")
      }
      head(etaDS)

      etaxv = bwplot(theForm, data = etaDS,
        panel = function(x,y,...)
        {
          panel.bpplot(x,y,...)
          panel.abline(v=0,lty = 2)
          yyy = tapply(x, as.integer(as.factor(format(y))), median)
          xxx = names(yyy)
          llines(xxx, yyy, lty = 2)
        },
        ylab = list(xvar.label, cex = 1.25),
        xlab = list("Eta Value", cex = 1.25),
        aspect = asp.eta.v.var
      )

   trellis.strip.color()

   if (print.eta.norm & print.eta.v.var) {
      plot(qqplot, split = c(1,1,2,1), more = TRUE)
      plot(etaxv,  split = c(2,1,2,1), more = FALSE)
   }
   else if (print.eta.norm) {
      plot(qqplot)
   }
   else if (print.eta.v.var) {
      plot(etaxv)
   }

   if(output) return(ds)

}
