# name:     nlme.modeltrail
# purpose:  creates run record of nlme models run with nlme.run
# input:    vector of nlme model separated by a comma (no need to use quotes)
# output:   data.frame with runrecord
# note:     relies on input with nlme.run

# ROXYGEN Documentation
#' Model trail / Runrecord for series of nested nlme models
#' @description create a model trial (or runrecord) for a series of nested models. This requires A problem statement as well as a reference model to be added to the nlme object. This is ensure by using qPharmetra's nlme.run instead of nlme.
#' @param ... nlme object names separated by a comma
#' @return data.frame containing the nlme model trail
#' @seealso \code{\link{nlme.run}}
#' @export nlme.modeltrail
#' @importFrom nlme anova.lme
#' @examples
#' library(nlme)
#' pkpdData = example.pkpdData()
#' EFF.1comp.1abs = function(dose, tob, cl, v, ka, keo)
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
#' fit.PD001.nlme = nlme.run(
#'    model = value ~ base * exp(base.eta) ,
#'    data = subset(pkpdData,type == "PD" & dose > 0 & value > 0.5),
#'    fixed = base ~ 1,
#'    random = base.eta  ~ 1,
#'    groups = ~ id,
#'    start = c(base = 1),
#'    problem = "Baseline Only Model",
#'    reference = 0
#' )
#' fit.PD002.nlme = nlme.run(
#'    model = value ~ base * exp(base.eta) + tSlope*time,
#'    data = subset(pkpdData,type == "PD" & dose > 0 & value > 0.5),
#'    fixed = base + tSlope ~ 1,
#'    random = base.eta  ~ 1,
#'    groups = ~ id,
#'    start = c(base = 1, tSlope = 0.01),
#'    problem = "Linear Time-Slope Model",
#'    reference = 1)
#' fit.PD003.nlme = nlme.run(
#'    model = value ~ base * exp(base.eta) + tSlope*time + dSlope * dose,
#'    data = subset(pkpdData,type == "PD" & dose > 0 & value > 0.5),
#'    fixed = base + tSlope + dSlope ~ 1,
#'    random = base.eta  ~ 1,
#'    groups = ~ id,
#'    start = c(base = 1, tSlope = 0.01, dSlope = 0.01),
#'    problem = "Linear Time-Slope+Dose-Slope Model",
#'    reference = 2)
#' fit.PD004.nlme = nlme.run(
#'    model = value ~ base + EFF.1comp.1abs(dose, time, cl * exp(cl.eta), v, ka, keo),
#'    data = subset(pkpdData,type == "PD" & dose > 0 & value > 0.5),
#'    fixed = base + cl + v + ka + keo ~ 1,
#'    random = cl.eta ~ 1,
#'    groups = ~ id,
#'    start = c(base = 1, cl = 1, v = 10, ka = 1, keo = 0.01),
#'    problem = "True Model",
#'    reference = 3)
#' nlme.modeltrail(fit.PD001.nlme,fit.PD002.nlme,fit.PD003.nlme,fit.PD004.nlme)
nlme.modeltrail = function(...)
{
  models = as.list(match.call())[-1] #models = as.list(c('fit.PD001.nlme','fit.PD002.nlme','fit.PD003.nlme','fit.PD004.nlme'))
  nams = unlist(lapply(models, as.character))

  ## check if the models were run with nlme.run
  test = lapply(models, function(x)
  {
    X = eval(parse(text = paste(x, "$object", sep = "")))
    is.list(eval(parse(text = x))) & all(names(eval(parse(text = x))) %in% c("object", "problem", "reference"))
  })
  if(any(unlist(test)==FALSE))
  {
    cat(paste(nams[unlist(test == FALSE)], collapse = "\n"))
    cat("... do(es) not have elements 'object', 'model', 'reference'")
  }

  ## get reference order
  reference.call = unlist(lapply(unlist(nams),
    function(nams) eval(parse(text = paste(nams, "$reference", sep = "")))))

  if(max(reference.call)>length(reference.call))
    stop("the maximum of any reference number cannot be higher than the number of models. check the 'reference' component of all objects")

  ## get problem statements
  problem.call = unlist(lapply(nams,
    function(nams) eval(parse(text = paste(nams, "$problem", sep = "")))))

  anova.call = paste("anova(", paste(paste(nams, "$object", sep = ""),collapse = ","), ")", collapse = "", sep = "")
  anova.call = parse(text = anova.call)
  myAnova = eval(anova.call[[1]])

  myAnova = as.data.frame(myAnova)
  myAnova = myAnova[, c('Model', 'df', 'logLik')]
  myAnova$OFV = -2 * myAnova$logLik
  myAnova$Model.Reference = c("-", as.character(reference.call[-length(reference.call)]))

  tmp = data.frame(ofv = myAnova$OFV, ref = reference.call)
  tmp2 = apply(cbind(c(NA, tmp$ofv[tmp$ref]),tmp$ofv), 1, diff)
  myAnova$delta.OFV = c("n.a.", round(tmp$ofv[-1]-tmp$ofv[reference.call], 1))
  myAnova$logLik = NULL

  myAnova$Comment = problem.call
  return(myAnova)
}


