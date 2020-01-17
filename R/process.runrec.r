# ROXYGEN Documentation
#' Format runrecord for reports
#' @description Process the run record into a data frame ready for integration into LaTeX report. Note that the function assumes runrecord has been run with PsN option -maxlvl = 0 and the function depends on input from \code{read.runrec}
#' @param runrec outut from \code{read.runrec}
#' @param improvement a list with elements value (numeric) and color (character) to emphasize significant improvements. Value defaults to -6.63, for a p<0.01 improvement for a single parameter added.
#' @param carryAlong any additional variable to be included (would need to be present in \code{runrec})
#' @param plain optional toggle to create a non-LaTeX formatted table
#' @return Either a LaTeX preformatted runrecord table (default) or a fully processed runrecord table without LaTeX preformatting (for inclusion into Microsoft products).
#' @export
#' @seealso \code{\link{read.runrec}}, \code{\link{process.parTable}}
#' @examples
#' rr = read.runrec(
#'  filename = "AAruninfo.txt",
#'  path = system.file(package = 'qpToolkit','NONMEM')
#' )
#' process.runrec(rr)
#' process.runrec(rr, plain = TRUE)
process.runrec <- function(
  runrec,
  improvement = list(value = -6.63, color = "blue"),
  carryAlong = NULL, plain = FALSE)
{
  runrec = runrec[, c('Run','Ref','OFV','dOFV','CondNum','Minimization','Description', carryAlong)]
  runrec$CondNum = round(runrec$CondNum)
  runrec$CondNum[runrec$CondNum>10000 & !is.na(runrec$CondNum)] =
    rep("$>$10,000", sum(runrec$CondNum>10000 & !is.na(runrec$CondNum)))
  runrec$Ref[is.na(runrec$Ref)] = "-"
  runrec$CondNum[is.na(runrec$CondNum)] = rep("-", sum(is.na(runrec$CondNum)))
  runrec = runrec[order(runrec$Run), ]
  runrec$Description = as.character(runrec$Description)
  runrec$Description = sub("~","vs.", runrec$Description)

  improved = !is.na(runrec$dOFV) & asNumeric(runrec$dOFV, "NA") < improvement$value
  runrec$OFV = round(runrec$OFV,1)
  runrec$dOFV = round(runrec$dOFV,1)
  if(!plain) {runrec$dOFV[improved] = paste(paste("\\textbf{\\color{",improvement$color,"}{", sep = ""),
                                            runrec$dOFV[improved],"}}",sep = "")}
  runrec$dOFV[is.na(runrec$OFV)] = rep("~", sum(is.na(runrec$OFV)))
  runrec$action = c("~", rep("rejected", (nrow(runrec)-1)))
  runrec$action[improved] = rep("accepted", sum(improved))
  myRunrec = runrec
  if(!plain) names(myRunrec)[names(myRunrec) == "dOFV"] = "$\\Delta$OFV"
  return(myRunrec)
}

if(F)
{
  ## important - assumes runrecord has been run with maxlvl = 0

}
