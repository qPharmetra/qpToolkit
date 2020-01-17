# name:     nlme.vpc
# purpose:  creates a VPC object of an nlme object
# input:    nlme.object, covariates to run VPC across
# output:   list with nlme object, the covariates inputted, the vpc results summarized and (optional) the vpc samples
# note:     to be plotted with nlme.vpcplot

# ROXYGEN Documentation
#' Create VPC output of an nlme object
#' @description Creates a VPC object of an nlme object ready for plotting
#' @param object an nlme object
#' @param nrep number of samples (defaulting to 10)
#' @param covariates covariates (like time and dose) to be used with the graph
#' @param fun function used to summarize simulated output. Defaults to smedian.hilow from the Hmisc package.
#' @param newdata if supplied allows simulating a VPC for another data set than the model data set. This dataset does need the dependent variable, independent variable, grouping variable, and any other covariate used in the model.
#' @param return.samp if T returns the non-summarized predictions as well
#' @return a list with
#' @export nlme.vpc
#' @importFrom Hmisc smedian.hilow
#' @examples
#' library(nlme)
#' pkpdData = example.pkpdData()
#'  EFF.1comp.1abs <- function(dose, tob, cl, v, ka, keo)
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
#'  fit.PD004.nlme = nlme.run(
#'    model = value ~ base + EFF.1comp.1abs(dose, time, cl * exp(cl.eta), v, ka, keo),
#'    data = pkpdData[pkpdData$type == "PD" & pkpdData$dose > 0 & pkpdData$value > 0.5, ],
#'    fixed = base + cl + v + ka + keo ~ 1,
#'    random = cl.eta ~ 1,
#'    groups = ~ id,
#'    start = c(base = 1, cl = 1, v = 10, ka = 1, keo = 0.01),
#'    problem = "True Model",
#'    reference = 4)
#'  summary(fit.PD004.nlme$object)
#'  nlme.extract(fit.PD004.nlme$object)$table
#'  vpc.PD004.nlme = nlme.vpc(fit.PD004.nlme$object, nrep = 100)
#'  nlme.vpcplot(fit.PD004.nlme$object, vpc.PD004.nlme)

nlme.vpc <- function(object, nrep = 10, covariates, fun = smedian.hilow, newdata = NULL, return.samp = FALSE)
{
	#require(Hmisc)

	## check input

	## run the simulation
	vpc.object = sapply(1:nrep, function(x, object, newdata)
	{
		if(is.null(newdata))  tmp = nlme.extract(object, method = "samples")
		if(!is.null(newdata)) tmp = nlme.extract(object, method = "samples", newdata = newdata)
		ypr = eval(getCovariateFormula(object)[[2]], tmp)
		return(ypr + tmp$residual)
	}, object = object, newdata = newdata)

	if(missing(covariates)) covariates = nlme.extract(object)$dataVars
	if(is.null(newdata))   theList = lapply(covariates, function(x, DF) eval(as.name(x), DF), DF = getData(object))
	if(!is.null(newdata))  theList = lapply(covariates, function(x, DF) eval(as.name(x), DF), DF = newdata)

	names(theList) = covariates

	qsObject = Hmisc::summarize(vpc.object, theList, smedian.hilow, stat.name = 'Central')
	iqr = Hmisc::summarize(vpc.object, theList, fun, conf.int = 0.75, stat.name = 'Central')
	names(iqr)[names(iqr)%in% c('Lower','Upper')] = c('LowerQ', 'UpperQ')
	qsObject = cbind(qsObject, iqr[, c('LowerQ', 'UpperQ')])

	## sort covariates by default so that time is first
	if(any(covariates %in% "time")) covariates = c("time", covariates[covariates %nin% "time"])

	if(!return.samp) return(list(object = object, covariates = covariates, vpc.summary = qsObject))
	if(return.samp)	 return(list(object = object, covariates = covariates, vpc.summary = qsObject, vpc.samp = vpc.object))
}



