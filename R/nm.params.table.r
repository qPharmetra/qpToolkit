# name:     nm.params.table
# purpose:  creates a parameter table
# input:    run number, optional: path if that's any different from "../WorkArea/NONMEM"
# output:   data frame
# note:

# ROXYGEN Documentation
#' Create NONMEM Parameter Estimate Table
#' @param run run rootname (e.g. run1)
#' @param path directory where rootname.ext resides
#' @param file.ext file extentions of the .ext file. Defaults to .ext
#' @param runIndex which (numeric) estimation method needs to be tabulated? Defaults to the last estimation method outputted to the .ext file
#' @param fixed.text character string to replace the entries for standard error and coefficient of variation (CV) for parameters that were fixed in the control stream
#' @param return.all logical indicator to return the standard data set for inclusion in reports (default) or if TRUE the data set with everything.
#' @return data frame with parameter name, estimate, coefficient of variation, standard error, and estimated/fixed information.
#' @export
#' @seealso \code{\link{process.parTable}}
#' @importFrom dplyr left_join
#' @examples
#' nm.params.table(run = "example1", path = getOption("qpExampleDir"))
#' nm.params.table("example2",  path = getOption("qpExampleDir"),
#' fixed.text = "(fixed to 0)", return.all = TRUE)
nm.params.table <- function(
  run,
  path = getOption("nmDir"),
  file.ext = ".ext",
  runIndex,
  fixed.text = ".....",
  return.all = FALSE
)
{
  flag = 0

  ## in this part the 6th summary row is extracted
  theExt = read.ext(run = run, path = path, file.ext = file.ext)
  theExt = lapply(theExt, function(x) x[x$ITERATION< -100000000,])
  theTab = lapply(theExt, t)
  theTab = lapply(theTab, function(x)
    {
    x = data.frame(x)
    names(x) = paste0("V", x["ITERATION",]+1e9)
    names(x) = swap(names(x)
                    , paste0("V",0:-6)
                    , c("estimate","se","Eigen","CondNum","sd.var","sd.sd.var","estimated")
    )
    x$Parameter = row.names(x)
    x$Parameter = sub("[,]",".",sub("[)]","",(sub("[(]","",x$Parameter))))
    return(x)
    }
  )
  runIndex = if(missing(runIndex)) length(theTab) else runIndex
  theTab = theTab[[runIndex]]

  check = lapply(theExt, function(x) x[x$ITERATION == "-1000000006",])[[runIndex]]
  if(nrow(check) == 0){
    flag = 1
    check[1,] = rep(0, ncol(check))
    message("nm version used is probably lower than 7.3.0.")
    message("Cannot reliably determine fixed/estimated thus making educated guesses.")
    message("Please check result carefully.")
  }
  check = check[-c(1,length(check))]
  check = data.frame(t(check))
  check$Parameter = sub("[,]",".",sub("[)]","",(sub("[(]","",row.names(check)))))
  names(check)[1] = "estimated"

  theTab$index = extract.number(theTab$Parameter)
  theTab$level = unlist(lapply(sapply(theTab$Parameter,function(x) extract.character(x)),paste, collapse = ""))
  parTab = theTab[theTab$level %in% c('THETA','SIGMA','OMEGA'),]
  parTab$ord = swap(parTab$level,c('THETA','OMEGA','SIGMA'), 1:3)
  parTab$ord = paste(parTab$ord,format(parTab$index), sep = "")
  parTab = parTab[order(parTab$ord),]
  parTab = reorder(parTab, "Parameter")

  ## merge the fixed / non-fixed information in
  parTab = parTab[, names(parTab)[names(parTab) != "estimated"]]
  if(nrow(parTab) != nrow(check))warning('numbers of rows do not match')
  parTab = dplyr::left_join(parTab, check)
  estimated = parTab$estimated == 0
  if(flag == 1) estimated[asNumeric(parTab$se[estimated]) > 1e+9] = FALSE
  #if no se returned set it here
  if(!"se" %in% names(parTab)) parTab$se = NA
  parTab$prse = NA
  parTab$prse[estimated] = parTab$se[estimated]/parTab$estimate[estimated] *100
  parTab$se[!estimated] = parTab$prse[!estimated] = fixed.text
  parTab$estimated = ifelse(estimated,"estimated","fixed")
  parTab$Run = rep(run, nrow(parTab))
  # don't use %in%, it is order dependent and we may name things out of order
  names(parTab)[match(c("Parameter", "estimate",  "se", "estimated", "prse"), names(parTab))] =
    c("Parameter","Estimate","SE","estimated","CV.perc")
  row.names(parTab) = 1 : nrow(parTab)

  parTab = if(!return.all){
    parTab = parTab[, c("Parameter","Estimate","CV.perc","SE","estimated")]
  } else {
    parTab
  }
  return(parTab)
}

