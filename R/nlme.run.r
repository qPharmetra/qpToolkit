# ROXYGEN Documentation
#' Run an nlme object augmented with model trail info
#' @description wrapper function for \code{nlme} model "method", and which adds a problem description (like $PROB in NONMEM) and the number of a reference model, the numeric position in the model building trail. This is intended to help building a model trail output in conjunction with \code{nlme.modeltrail}.
#' @param ... a vector consisting of an nlme call, a problem statement, and a reference number (to which the run will be compared)
#' @return a list with the nlme object and the assocated proble statement and reference number
#' @export nlme.run
#' @importFrom nlme nlme
#' @seealso \code{\link{nlme.modeltrail}}, \code{\link{nlme.vpc}}
#' @examples
#' library(nlme)
#' fm1 = nlme(height ~ SSasymp(age, Asym, R0, lrc),
#'    data = Loblolly,
#'    fixed = Asym + R0 + lrc ~ 1,
#'    random = Asym ~ 1,
#'    start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
#' summary(fm1)
#' fm2 = update(fm1, random = pdDiag(Asym + lrc ~ 1))
#' summary(fm2)
#'
#' # Now the same thing but using nlme.run()
#' fm1 = nlme.run(height ~ SSasymp(age, Asym, R0, lrc),
#'                data = Loblolly,
#'                fixed = Asym + R0 + lrc ~ 1,
#'                random = Asym ~ 1,
#'                start = c(Asym = 103, R0 = -8.5, lrc = -3.3),
#'                problem="Initial model",
#'                reference=0
#' )
#' summary(fm1$object) # note one must specify "$object" here
#' fm1$problem
#' #"Initial model"
#' fm1$reference
#' #0
#' fm2 = nlme.run(height ~ SSasymp(age, Asym, R0, lrc),
#'                data = Loblolly,
#'                fixed = Asym + R0 + lrc ~ 1,
#'                random = pdDiag(Asym + lrc ~ 1),
#'                start = c(Asym = 103, R0 = -8.5, lrc = -3.3),
#'                problem="Different random effect",
#'                reference=1
#' )
#' summary(fm2$object)
#' fm2$problem
#' #"Different random effect"
#' fm2$reference
#' #1

nlme.run = function(...)
{
  nlme.arguments = as.list(match.call())
  if(any(c('problem', 'reference') %nin% names(nlme.arguments))) stop("need problem and reference input")
  problem = nlme.arguments$problem
  reference = nlme.arguments$reference
  nlme.arguments$problem = nlme.arguments$reference = NULL
  nlme.object = do.call("nlme", nlme.arguments[-1])
    ## the -1 is to remove an obsoletly parsed "" element at position 1 in the list
  return(list(object = nlme.object, problem = problem, reference = reference))
}
