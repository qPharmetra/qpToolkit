# name:     nm.process.scm
# purpose:  read and process SCM output file
# input:    path where the completed SCM output resides
# output:   table in latex format
# note:     function only produces the table latex output is supported

# ROXYGEN Documentation
#' Parsing SCM output
#' @description Read and process SCM output file such that the output can be directly copied into Microsoft products and LaTeX typesetting.
#' @param path full directory path where scmlog files reside
#' @param digits number of significant digits
#' @return A character vector with processed SCM output, ready for inclusion into a (LaTeX) report
#' @export
#' @importFrom utils head
#' @importFrom utils tail
#' @examples
#' library(xtable)
#' ## the code below works if a number of scmlog.txt files are in folder NONMEM/scm files
#' myPath = file.path(getOption("qpExampleDir"),"scm_example2")
#' scm = nm.process.scm(myPath)
#' names(scm)
#'
#' # Fail if scm file not found
#' foo <- try(nm.process.scm(file.path(getOption("qpExampleDir"),"scm_example0")))
#'
#' scm$summary
#'
#' # populate an appendix with scm output
#' print(xtable(scm$scm.latex
#'    , caption = "Full Stepwise Covariate Building output"
#'    , label =  'Tab:scmfull')
#'       , sanitize.text.function = function(x)x
#'       , include.rownames = FALSE
#'       , size = "small"
#'       , caption.placement ="top"
#'       , booktabs = TRUE
#'       , tabular.environment = "longtable"
#'       , floating = FALSE
#'       #  , only.contents = TRUE
#' )

nm.process.scm <- function(path,digits = 3)
   #path = paste(getOption("qpExampleDir"), "scm files","Klas", sep="/")
{
  scmpath = paste(path, "scmlog.txt", sep="/")
  if(!file.exists(scmpath))stop('cannot find ', scmpath)
  scm = readLines(scmpath)
  scmshortpath = paste(path, "short_scmlog.txt", sep="/")
  if(!file.exists(scmshortpath))stop('cannot find ', scmshortpath)
  scmshort = readLines(scmshortpath)
  model = which(substring(scm,1,5) == "MODEL")
  nothing = which(scm == "")
  nothing = sapply(model, function(x,y) y[y>x][1], y = nothing)

  scmTITLE = scm[model[1]]

  fullSCM = lapply(seq(along=model), function(x,start, end, scm){
    as.character(scm[(start[x]+1):(end[x]-1)])
  }
  , start = model
  , end = nothing
  , scm = scm
  )

  # unravel the elements of each row and make them have fixed length
  unravel.scm <- function(x)
  {
     c(substring(x,1,17),substring(x,18,21),substring(x,22,34),substring(x,35,47)
       ,substring(x,48,67),substring(x,68,70),substring(x,71,80),substring(x,81,85)
       ,substring(x,86,97),substring(x,98))
  }
  fullSCM = lapply(fullSCM, function(y) sapply(y, function(x) unravel.scm(x)))
  fullSCM = lapply(fullSCM, function(y) apply(y, 2, function(x) trimSpace(x)))
  scmlist2df = function(x) if(length(x)==10) data.frame(matrix(c(x), nrow=1)) else data.frame(apply(x, 1, t))
  fullSCM = lapply(fullSCM, scmlist2df)

  ## test if the SCM did not lead to any inclusion of any covariate
  ## this can happen when running SCMs across mutliple (simulated) datasets
#   test.it = unlist(lapply(fullSCM, function(x) any(as.character(x) == "NO")))
#   if(any(test.it)){
#     fullSCM = fullSCM[!test.it]
#     fullSCM = data.frame(lapply(fullSCM, function(x) do.call("rbind",x))[[1]])
#     fullSCM[,6] = paste("$",fullSCM[,6],"$ ",signif(as.numeric(fullSCM[, 7]),3),sep="")
#     ok = isNumeric(fullSCM[,10])
#     fullSCM[ok,10] = substring(sprintf("%4f",signif(asNumeric(fullSCM[ok,10]),4)),1,6)
#     fullSCM[,10][fullSCM[,10] == "0.0000"] = "$<$0.0001"
#     fullSCM[,10][fullSCM[,10] == "9999.0"] = "$>$0.9999"
#     fullSCM = fullSCM[,-7]
#     fullSCM = rbind(fullSCM, data.frame(matrix(c("$\\textbf{NOTHING ADDED}$",rep("",9)), nrow=1))[,-7])
#
#     names(fullSCM)=c("MODEL", "TEST", "OFV$_{base}$","OFV$_{test}$","$\\Delta$OFV","GOAL","$\\Delta$DF","SIGNIFICANT", "P value")
#     return(fullSCM)
#     } else fullSCM = lapply(fullSCM, function(x) data.frame(do.call("rbind", x)))

  nFwd = grep("Parameter-covariate relation chosen in this forward step:", scm)
  nBwd = grep("Parameter-covariate relation chosen in this backward step:", scm)

  length(fullSCM)
  names(fullSCM) = c(
    paste("START FORWARD STEP",(1:length(nFwd)))
    , paste("START BACKWARD STEP",(1:length(nBwd)))
  )

  ## apply round(x,1) to OBJV values
  fullSCM = lapply(fullSCM, function(x)
  {
    x$X3 = sprintf("%.1f",as.numeric(x$X3))
    x$X4 = sprintf("%.1f",as.numeric(x$X4))
    x$X5 = sprintf("%.1f",as.numeric(x$X5))
    x$X7 = sprintf("%.2f",as.numeric(x$X7))
    return(x)
  })

  ## provide data frame names
  scmNames = c("MODEL", "TEST", "BASE.OFV", "NEW.OFV", "TEST","OFV.DROP", "GOAL", "dDF", "SIGNIFICANT", "PVAL")
  fullSCM = lapply(fullSCM, function(x, nams) {names(x) = nams; return(x)}
         , nams =  scmNames)

  ### ---- Summary and model from SCM short ----

  relations =  which(substring(scmshort,1,5) == "Relat")
  scm.summary = scmshort[1:(relations[1]-1)]
  scm.summary = sapply(scm.summary, function(x) unravel.scm(x))
  scm.summary = apply(scm.summary, 2, function(x) trimSpace(x))
  scm.summary = scmlist2df(scm.summary)
  names(scm.summary) = scmNames
  dashes =  which(substring(scmshort,1,5) == "-----")
  final.model = scmshort[(tail(relations,1)+1):(tail(dashes,1)-1)]
  final.model = sapply(final.model, function(x) unlist(strsplit(x, "\\s+")))
  final.model = lapply(final.model, function(x){
     data.frame(Parameter=x[1],Covariates=paste(if(length(x)>1) x[-1] else "", collapse = ", "))
  })
  final.model = do.call("rbind",final.model)
  row.names(final.model)=NULL

  ## find what relations were kept
  steps = grep("Relations included after final step:", scmshort)
  seltxt = "Parameter-covariate relation chosen in this forward step: "
  forwardSteps = sub(seltxt,"",scm[grep(seltxt,scm)])
  seltxt = "Parameter-covariate relation chosen in this backward step: "
  backwardSteps = sub(seltxt,"",scm[grep(seltxt,scm)])
  allSteps = c(paste("ADDED",forwardSteps), paste("REMOVED",backwardSteps))
  allSteps[allSteps == "ADDED --"] = "NOTHING ADDED"
  allSteps[allSteps == "REMOVED --"] = "NOTHING REMOVED"

  step.n=length(nFwd)+length(nBwd)
  step.labels = character(step.n)
  step.action = allSteps
  for(i in 1:step.n){
     step.labels[i]=ifelse(i<=length(nFwd),
                           paste("START FORWARD STEP",i),
                           paste("START BACKWARD STEP",i-length(nFwd)))
  }
  steps=list()
  for(i in 1:step.n){
     steps[[i]]=list(label=step.labels[i], runs=fullSCM[i], action=step.action[i])
  }

  ### ---- Perform LaTeX work on fullSCM ----

  ## define an empty row
  empty.row = fullSCM[[1]][1,]
  empty.row[1,] = rep("~", ncol(empty.row))

  ## insert the emtpy row and label it FORWARD or BACKWARD
  fullSCM.latex = lapply(1:length(fullSCM), function(x,er,allSteps,fullSCM)
  {
    ER = rbind(er, er, er)
    ER[2,1] = allSteps[x]
    ER[1,1] = names(fullSCM)[x]
    ER[3,1] = "\\cmidrule{2-8}%"
    y=data.frame(rbind(ER[1,], fullSCM[[x]], ER[2,],ER[3,]))
    names(y) = names(fullSCM[[x]])
    return(y)
  }, er = empty.row, fullSCM = fullSCM, allSteps = allSteps
  )
  fullSCM.latex = data.frame(do.call("rbind", fullSCM.latex))

  names(fullSCM.latex) = c("MODEL","TEST","BASE.OFV", "NEW.OFV","DIFF","OFV.DROP","GOAL","dDF",
                     "SIGNIFICANT","PVAL")
  head(fullSCM.latex)

  ## make fwd/bckwd italics
  msel.fwd = grep("FORWARD", fullSCM.latex$MODEL)
  msel.bwd = grep("BACKWARD", fullSCM.latex$MODEL)
  msel.add = grep("ADDED", fullSCM.latex$MODEL)
  msel.rem = grep("REMOVED", fullSCM.latex$MODEL)
  msel = c(msel.fwd,msel.bwd,msel.add,msel.rem)
  fullSCM.latex$MODEL[msel] = paste("$\\textit{",fullSCM.latex$MODEL[msel],"}$", sep = "")

  ## remove 'NO'
  fullSCM.latex$SIGNIFICANT[fullSCM.latex$SIGNIFICANT == "NO"] = "~"
  fullSCM.latex$OFV.DROP = paste("$",fullSCM.latex$OFV.DROP,"$", sep = "")
  ok = isNumeric(fullSCM.latex$GOAL)
  fullSCM.latex$GOAL[ok] = signif(asNumeric(fullSCM.latex$GOAL[ok]),digits)
  fullSCM.latex$GOAL = paste(fullSCM.latex$OFV.DROP,fullSCM.latex$GOAL)
  fullSCM.latex$OFV.DROP = NULL

  ok = isNumeric(fullSCM.latex$PVAL)
  fullSCM.latex$PVAL[ok] = substring(sprintf("%4f",signif(asNumeric(fullSCM.latex$PVAL[ok]),digits)),1,6)
  fullSCM.latex$PVAL[fullSCM.latex$PVAL == "0.0000"] = "$<$0.0001"
  fullSCM.latex$PVAL[fullSCM.latex$PVAL == "9999.0"] = "$>$0.9999"

  aadp = align.around.decimal.point
  fullSCM.latex$BASE.OFV[ok] = aadp(round(asNumeric(fullSCM.latex$BASE.OFV[ok]), 1), len=max(5,max(nchar(fullSCM.latex$BASE.OFV[ok]))-2))
  fullSCM.latex$NEW.OFV[ok] = aadp(round(asNumeric(fullSCM.latex$NEW.OFV[ok]), 1), len=max(5,max(nchar(fullSCM.latex$NEW.OFV[ok]))-2))
  fullSCM.latex$DIFF[ok] = aadp(round(asNumeric(fullSCM.latex$DIFF[ok]), 2)
                          , len=max(nchar(round(extract.number(fullSCM.latex$DIFF[ok])))))
  fullSCM.latex$TEST[fullSCM.latex$TEST =="PVAL"] = "~"

  names(fullSCM.latex) = c("MODEL", "TEST", "OFV$_{base}$","OFV$_{test}$","$\\Delta$OFV","GOAL","$\\Delta$DF","SIGNIFICANT", "P value")
  fullSCM.latex$TEST = NULL

  SCM = structure(list(  full.scm = fullSCM
             , summary=scm.summary
             , model = final.model
             , steps = steps
             , scm.latex = fullSCM.latex
  ), class="SCM")
  return(SCM)
}
