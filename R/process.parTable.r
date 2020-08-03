globalVariables('logit.inv')
#' Format parameter estimate table
#' @description Process parameter table for inclusion into LaTeX, or MS Excel or Word
#' @param nm output from \code{nm.params.table}
#' @param index which estimation (only relevant in case of multiple \code{$EST})
#' @param ci confidence interval for 95\% CI
#' @param formatted whether to enforce length of significant digits, see \code{\link{formatted.signif}}
#' @param latex passed to \code{\link{formatted.signif}}
#' @param align.dot passed to \code{\link{formatted.signif}}
#' @param digits number of significant digits in output
#' @param transformations a list with elements log or logit, that, in turn, are numeric vectors representating the THETA numbers that are logged or logit-transformed, respectively.
#' @param Descriptor vector of descriptor (alias) names for numbered parameter estimates. For example c("CL","V1","SD(CL)","cor(CL,V1)","SD(V1)","prop. residual var.")
#' @param plain logical (defaults to F) indicating if LaTeX formatting of the table should be skipped, allowing the result to be easily available for Microsoft products.
#' @param missing.format what to replace non-estimated / fixed parameter estimates, or
#' @param remove.fixed.sigma logical option to remove sigma estimates from the table
#' @return Either a LaTeX preformatted parameter estimate table (default) or a fully processed parameter estimate table without LaTeX preformatting (for inclusion into Microsoft products).
#' @export
#' @seealso \code{\link{nm.params.table}}, \code{\link{process.runrec}}
#' @importFrom stats qt
#' @examples
#' process.parTable(
#'    nm = nm.params.table(run = "example1", path = getOption("qpExampleDir"), runIndex = 1)
#'  , transformations = list(log = c(1:12))
#'  , missing.format = "...."
#' )
#' process.parTable(
#'    nm = nm.params.table(run = "example1", path = getOption("qpExampleDir"))
#'  , transformations = list(logit = c(7,9))
#'  , plain = TRUE
#'  , missing.format = "...."
#' )
#' process.parTable(
#'    nm = nm.params.table(run = "example1", path = getOption("qpExampleDir"))
#'  , transformations = list(logit = c(7,9))
#'  , plain = TRUE
#'  , formatted = FALSE
#'  , missing.format = "...."
#' )

process.parTable <- function(
  nm
  , index = 1
  , ci = 0.95
  , digits = 3
  , formatted = TRUE
  , latex = FALSE
  , align.dot = FALSE
  , transformations
  , Descriptor
  , plain = FALSE
  , missing.format = "....."
  , remove.fixed.sigma = FALSE
){
    mysig <- function(x, digits, formatted, latex, align.dot, ...){
      stopifnot(is.logical(formatted), length(formatted) == 1)
      if(formatted) return(formatted.signif(x, digits = digits, latex = latex, align.dot = align.dot))
      if(!formatted)return(signif(x, digits = digits))
    }
    if(is.data.frame(nm))
    {
      parTab = nm
      parTab$SE[parTab$estimated == "fixed"] = "1e+10"
    } else   {
      ## assume nm.extract.xml output
      parTab = nm$table[[index]]
    }
    okEst = parTab$estimated == "estimated"
    parTab$Estimate = asNumeric(parTab$Estimate)

    #parTab = parTab[parTab$Estimate != 0, ]
    lenTheta = length(which(grepl("THETA",parTab$Parameter)))
    lenOmega = length(which(grepl("OMEGA",parTab$Parameter)))
    lenSigma = length(which(grepl("SIGMA",parTab$Parameter)))

    parTab$Parameter = as.character(parTab$Parameter)
    parTab$CV.perc[okEst] = abs(round(asNumeric(as.character(parTab$CV.perc[okEst])),1))

    skipTrans = FALSE ## default
    if(!all(is.na(parTab$SE)))
    {
      cilo = parTab$Estimate[okEst] + qt(p =(1-ci)/2, df = 1e5) * asNumeric(parTab$SE[okEst])
      ciup = parTab$Estimate[okEst] + qt(p =(1-((1-ci)/2)), df = 1e5) * asNumeric(parTab$SE[okEst])
      parTab$CI95 = rep("NC", nrow(parTab))
      parTab$CI95[okEst] = paste(
        "(",
        as.character(mysig(cilo,digits = digits, formatted = formatted, latex = latex, align.dot = align.dot))," - ", mysig(ciup,digits = digits, formatted = formatted, latex = latex, align.dot = align.dot),
        ")",
        sep = ""
      )
    } else {
      skipTrans = TRUE  ## toggle to later skip tranforming SE, 95%CI etc..
      parTab$SE = parTab$CI95 = parTab$CV.perc = rep("-", nrow(parTab))
    }

    myFun <- function(x, type)
    {
      switch(type,
             log = exp(x),
             log10 = 10^x,
             logit = logit.inv(x))
    }

    cvFun <- function(x,type)
    {
      switch(type,
             log = as.character(round(100*(exp(x)-1))),
             log10 = as.character(round(100*(10^x-1))),
             logit = "-" )
    }
    ## perform transformations if needed
    parTab$transformed = rep("no",nrow(parTab))
    if(!missing(transformations))
      if(is.list(transformations))
      {
        len = length(transformations)
        for(x in seq(len))
        {
          ok = transformations[[x]]
          nms = names(transformations)[x]
          parTab$transformed[ok] = rep(nms, length(ok))
          if(!skipTrans)
          {
            ciloth = myFun(parTab$Estimate[ok] + qt(p =(1-ci)/2, df = 1e5) *
                             asNumeric(parTab$SE[ok]), nms)
            ciupth = myFun(parTab$Estimate[ok] + qt(p =(1-(1-ci)/2), df = 1e5) *
                             asNumeric(parTab$SE[ok]), nms)
            parTab$CI95[ok] = paste("(",as.character(mysig(ciloth,digits = digits, formatted = formatted, latex = latex, align.dot = align.dot))," - ",
                                    mysig(ciupth,digits = digits, formatted = formatted, latex = latex, align.dot = align.dot), ")", sep = "")
            parTab$CV.perc[ok] = cvFun(asNumeric(parTab$SE[ok]), nms)
            #parTab$SE[ok] = 1e+10
          }
          parTab$Estimate[ok] = myFun(parTab$Estimate[ok],nms)
        }
      }

    ## format some layout things
    missSE = !okEst | parTab$transformed != "no"
    parTab$CV.perc[missSE] = missing.format
    parTab$Estimate = sprintf("%#.3g",mysig(parTab$Estimate, digits = digits, formatted = FALSE, latex = latex, align.dot = align.dot))
    if(!skipTrans) parTab$SE[okEst] = as.character(mysig(asNumeric(parTab$SE[okEst]), digits = digits, formatted = formatted, latex = latex, align.dot = align.dot))
    parTab$SE[missSE] = missing.format
    #parTab$CV.perc = ifelse(is.na(parTab$CV.perc), "NC", parTab$CV.perc)

    ## make greek symbols
    if(!plain){
      parTab$Parameter = casefold(parTab$Parameter, FALSE)
      isth = grepl("theta", parTab$Parameter)
      isom = grepl("omega", parTab$Parameter)
      issi = grepl("sigma", parTab$Parameter)

      parTab$Parameter[isth] =
        paste("theta", "_{", extract.number(parTab$Parameter[isth]), "}", sep = "")
      parTab$Parameter[isom] =
        paste("omega", "_{", extract.number(parTab$Parameter[isom]), "}", sep = "")
      parTab$Parameter[issi] =
        paste("sigma", "_{", extract.number(parTab$Parameter[issi]), "}", sep = "")

      parTab$Parameter[isth] = paste("$\\", parTab$Parameter[isth], "$", sep = "")
      parTab$Parameter[isom] = paste("$\\", parTab$Parameter[isom], "$", sep = "")
      parTab$Parameter[issi] = paste("$\\", parTab$Parameter[issi], "$", sep = "")

      #parTab$Parameter = sub("\\(","\\_{", parTab$Parameter)
      #parTab$Parameter[isth] = sub("\\)","\\}", parTab$Parameter[isth])
      #parTab$Parameter[isom] = sub("\\}","}\\^2", parTab$Parameter[isom])
      #parTab$Parameter[issi] = sub("\\}","}\\^2", parTab$Parameter[issi])

      parTab$Parameter = paste(parTab$Parameter, sep = "")
    } ## end if(!plain)


    ## Remove fixed sigma
    if(remove.fixed.sigma){
      parTab = parTab[!(grepl(".*sigma.*",parTab$Parameter)&asNumeric(parTab$Estimate) == 1&parTab$SE == 0),]
    }

    ## insert useful parameter names
    if(!missing(Descriptor)){
      if(length(Descriptor) == nrow(parTab))
      {
        parTab$Descriptor = Descriptor
        parTab = reorder(parTab, c('Parameter','Descriptor'))
      }
    }

    return(parTab)
  }

