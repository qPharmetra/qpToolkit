# name:     nlme.extract
# purpose:  extracts all relevant nlme attributes as a list
# input:    nlme object
# output:   list with relevant nlme output
# note:

##  optional: method = "samples" will create a simulated data set which will

# ROXYGEN Documentation
#' Extract information from nlme object
#' @description Given an nlme object, extract all available information for further processing. In addition parse the information into a ready-to-use parameter estimate table. Furthermore, when method is set to samples, it will create samples to be used for a VPC (see \code{nlme.vpc}).
#' @param obj an nlme object
#' @param method can be either components (extracting function) or samples (for VPC)
#' @param newdata a new data set with at the minimum the data items (independent, dependent variables, covariates, and grouping variables) required to build the nlme object.
#' @param sample logical (default is FALSE) indicating whether to sample or not
#' @param digits number of significant digits for parameter estimate table
#' @param digits.CV number of significant digits for coefficient of variation in the parameter estimate table
#' @return list with extracted nlme object information, model summary, model parameter table, or, when method = "samples", a matrix with predictions for data.frame \code{getData(obj)} or \code{newdata}.
#' @export nlme.extract
#' @seealso \code{\link{nlme.run}}, \code{\link{nlme.diag}}, \code{\link{nlme.vpc}},  \code{\link{nlme.simPars}}, \code{\link{nlme.getFixPars}}, \code{\link{nlme.getRanPars}}
#' @importFrom nlme getData
#' @importFrom nlme getCovariateFormula
#' @importFrom nlme fixef ranef
#' @importFrom nlme ranef
#' @importFrom nlme getResponseFormula
#' @importFrom nlme nlme
#' @importFrom nlme getGroupsFormula
#' @importFrom nlme intervals
#' @importFrom nlme pdMatrix
#' @importFrom nlme intervals
#' @examples
#' fm1 <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
#' data = Loblolly,
#' fixed = Asym + R0 + lrc ~ 1,
#' random = Asym ~ 1,
#' start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
#' fm1.out = nlme.extract(fm1)
#' fm1.out
nlme.extract = function(obj,
  method = "components",
  newdata = NULL,
  sample = FALSE,
  digits = 3,    ## number of significant digits for output tables using signif()
  digits.CV = 0  ## number of decimals reported for CV% values using round()
  )
{
  #require(MASS)
  #require(nlme)

  CLASS = class(obj)[1]
	## test if object is indeed nlme
	if(!CLASS %in% c("nlme", "glme"))
		stop(paste("object", deparse(substitute(object)), "must be of class 'nlme' or 'glme'"))

	## start with empty warning message
	WARNINGS = ""

	## test if we can use intervals to extract point estimates
	intervals.OK = TRUE
	if(obj$apVar[1] == "Non-positive definite approximate variance-covariance")
	{
		WARNINGS = c(WARNINGS, "\nrandom effects var/cov matrix non-positive / semi-definite. extracted point estimates only")
		intervals.OK = FALSE
	}

	## perform further checks on input
	if(!method %in% c("components", "samples"))
		stop("method must be either components or samples")
	if(!is.logical(sample))
		stop("sample must be logical (T or F)")
	if(is.null(newdata)) newdata = getData(obj)
	nr = nrow(newdata)

	form = nlme::getCovariateFormula(obj)[[2]]
	response = nlme::getResponseFormula(obj)[[2]]
	groups = nlme::getGroupsFormula(obj)[[2]]

	## we start with fixed effects
	fixef = list(fixef = nlme.getFixPars(obj))

	## this is where we process random effects from reStruct
	val = pdMatrix(obj$modelStruct$reStruct)

	## names of every single RE parameter in the model must be different
	## so list(trial = eta.base ~ 1, id = eta.base ~ 1) is not OK
	## must be: list(trial = eta.base.trial ~ 1, id = eta.base.id ~ 1)
	## note that K.Prins confirmed that the model result of both coding styles is exactly the same
	## check this and report problem if violated

	if(any(duplicated(unlist(lapply(val,names))))) stop("naming violation: two random parameters with the same name, rerun NLME object with separate RE names. See extract.nlme() function for details")

	## multiply diagonals by sigma squared to get estimates
	sig2 = obj$sigma ^2
	for(i in seq(along = val))
		val[[i]] = sig2 * val[[i]]
	val = lapply(val, function(x) sqrt(diag(x)))
	ranNames = unlist(lapply(val, names))

	## draw samples by inherent grouping classes
	tmp = val

	## note: could also do a draw from the uncertainy matrix apVar here
	## this is an in beta testing as correlation, block omega structures
	## can make things very complex and give potentially erroneous results

	## should use the work done in ranTab creation! to be done later

	ss1 = lapply(1:length(tmp), function(x, object, tmp, newdata)
	{
		samples = sapply(1:length(tmp[[x]]), function(X, object, tmp, newdata,x)
		{
			rnorm.by.id(eval(as.name(names(tmp)[x]),newdata), sd = tmp[[x]][X])
		}, object = object, tmp = tmp, newdata = newdata, x = x)
		return(samples)
	}, object = obj, tmp = tmp, newdata = newdata)

	ss2 = as.data.frame(do.call("cbind", ss1))
	names(ss2) = unlist(lapply(tmp, names))
	if(intervals.OK == TRUE) rvar = nlme.getRanPars(obj)$var
	if(intervals.OK == FALSE) rvar = NULL
	ranef = list(ranef = c(val, list(var=rvar)))

	## create sampling grid of fixed effects
	myfix = nlme.getFixPars(obj)$coef
	if(sample) myfix = nlme.simPars(nlme.getFixPars(obj), N = 1)
	fixDF = data.frame(matrix(rep(myfix, nr), ncol = length(fixef(obj)), byrow = TRUE))
	names(fixDF) = names(fixef(obj))
	ss2 = cbind(ss2, fixDF)

	## add explanatory covariates to data set
	dataVars = all.vars(form)
	dataVars = dataVars[!dataVars %in% c(names(fixef(obj)), ranNames)]
	ss2 = cbind(ss2, newdata[, dataVars])
  if(length(dataVars)==1) names(ss2)[length(names(ss2))] = dataVars

	## work on varStruct component
	pV <- obj$modelStruct$varStruct
	matchVar = match(class(pV)[1], c("NULL", "varPower", "varConstPower", "varFixed"), 0)
	if(matchVar == 0) WARNINGS = c(WARNINGS, "\nunmatched varStruct model!! only sigma provided!!")
	varCoef = stats::coef(pV)
  varForm = attr(obj$modelStruct$varStruct,"formula")
	residual = list(sigma = obj$sigma, varCoef = varCoef, varClass = class(pV), varFunc = varForm)
#      traceback()
	## this bit for samples across newdata
	nr = nrow(newdata)
	if(matchVar == 1) {
		ss2 = cbind(ss2, data.frame(residual = stats::rnorm(nr, 0, obj$sigma)))
	}
	if(matchVar == 2) {
		ss2 = cbind(ss2,
			data.frame(residual = stats::rnorm(nr, 0, obj$sigma * stats::predict(obj,newdata)^varCoef)))
	}
	if(matchVar == 3) {
		ss2 = cbind(ss2,
			data.frame(residual = stats::rnorm(nr, 0, varCoef[1] + obj$sigma * stats::predict(obj,newdata)^varCoef[2])))
	}
	if(matchVar == 4) {
	## this one needs testing!!!
	## evaluate varFixed formula across newdata
		WARNINGS = c(WARNINGS, "\nthe sampling routine was not yet validated for the variance function of varFixed class")
		varCovariate = eval(varForm[[2]], newdata)
		ss2 = cbind(ss2,
			data.frame(residual = stats::rnorm(nr, 0, varCovariate * obj$sigma)))
	}
	## also need to work on varIdent!!

	varef = list(varef = residual)

	## now work on corStruct
	pC <- obj$modelStruct$corStruct
	coref = pC

	## make final parameter table
	fullParTable = data.frame()
	if(method == "components"){

	fixTab = as.data.frame(summary(obj)$tTable)
	fixTab$Parameter = row.names(fixTab)
	fixTab = fixTab[, c("Parameter", "Value","Std.Error")]
	fixTab$Std.Error = signif(fixTab$Std.Error, digits)
	fixTab$Value = signif(fixTab$Value, digits)
	fixTab$Est.se = paste(fixTab$Value, " (", fixTab$Std.Error, ")", sep = "")
	fixTab = cbind(fixTab, data.frame(intervals(obj)$fixed[, c('lower','upper')]))
	fixTab$CI95 = paste("(",signif(fixTab$lower,digits), " - ", signif(fixTab$upper, digits), ")", sep = "")
   fixTab$CV = paste(round(abs(fixTab$Std.Error/fixTab$Value)*100,digits.CV), "%", sep = "")
   fixTab = fixTab[, c('Parameter', 'Est.se', 'CV', 'CI95')]
	fixTab

	if(intervals.OK)
	{
		simRE = nlme.simPars(nlme.getRanPars(obj), N = 1000)
		ranEst = attr(obj$apVar, "Pars")
		msel = c(grep("varStruct", names(ranEst)), grep("corStruct", names(ranEst)))
		if(length(msel) == 0)
		{
		 simRE = exp(simRE)
		 ranEst = exp(ranEst)
		}
		if(length(msel) > 0)
		{
		 simRE[,-msel] = exp(simRE[,-msel])
		 ranEst[-msel] = exp(ranEst[-msel])
		}
		SDs = apply(simRE, 2, function(x) sqrt(stats::var(x)))
	 	ranTab = data.frame(Parameter = names(attr(obj$apVar, "Pars")),
  		  Value = signif(ranEst, digits),
        Std.Error = signif(SDs, digits)
       )

    tmp = rbind(do.call("rbind",intervals(obj)$reStruct), intervals(obj)$sigma)
    row.names(tmp)[nrow(tmp)] = "sigma"
    ranTab = cbind(ranTab, tmp[, c('lower','upper')])
  	 ranTab$Est.se = paste(ranTab$Value, " (", ranTab$Std.Error, ")", sep = "")
  	 ranTab$CV = paste(round(abs(ranTab$Std.Error/ranTab$Value)*100,digits.CV), "%", sep = "")
  	 ranTab$CI95 = paste("(",signif(ranTab$lower,digits), " - ", signif(ranTab$upper, digits), ")", sep = "")
  	 ranTab = ranTab[, c('Parameter', 'Est.se', 'CV', 'CI95')]
    ranTab
	}

	if(intervals.OK == FALSE)
	{
		ranTab = data.frame(Parameter = names(unlist(val)),
			Est.se = unlist(val),
			Std.Error = "n.a.",
      CV = "n.a.",
      CI95 = "n.a.")
	}
	ranTab$Parameter = as.character(ranTab$Parameter)
	ranTab$Parameter[ranTab$Parameter == "lSigma"] = "sigma"

	fullParTable = data.frame(rbind(fixTab, ranTab))
	names(fullParTable)[names(fullParTable) == "Value"] = "Estimate"
	row.names(fullParTable) = 1:nrow(fullParTable)

	} ## end if method == "components"

	## concatenate all to become final output list
	general = list(form = form, response = response, groups = groups, dataVars = dataVars)
	val = c(general, fixef, ranef, coref, varef)

	## output the result
	WARNINGS = paste(WARNINGS, collapse = "")
	if(WARNINGS != "") {print(paste("warnings:", WARNINGS))}
	if(method == "components") return(c(val,list(table = fullParTable)))
		else (return(ss2))
}



