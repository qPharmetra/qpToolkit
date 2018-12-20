# ROXYGEN Documentation
#' Create a control stream for simulation
#' @description Parse random effects and construct new random effects statements
#' @param sw either "omega" or "sigma" for IIV or residual random effects
#' @param ctl character vector containing the current version of the NONMEM control stream
#' @param ranEf vector with the estimated random effects. These are the estimates to be placed in the new control stream
#' @return A control stream ready for execution written to file
#' @note This function is only used by \code{nm.create.control}.
#' @seealso \code{\link{nm.create.control}}
#' @export
#' @import Hmisc

makeRandom = function(sw="omega", ctl, ranEf){
  # sw   = either "omega" or "sigma" for IIV or residual random effects
  # ctl  = the character vector containing the current version of the NONMEM
  #        control stream
  # ranEf = vector with the estimated random effects. These are the estimates to be
  #         placed in the new control stream

  # Parse random effects and construct new random effects statements
  str = ifelse(sw=="omega", "\\$OMEGA", "\\$SIGMA")
  repl = ifelse(sw=="omega", "$OMEGA", "$SIGMA")

  nRan = length(grep(str,ctl))
  ran = vector("character", nRan)
  ivals = vector("integer", nRan) # ivals is the ending index for the random effects vector
                                  # sum(ivals) is equal to length(ranef)
  ranEf = ranEf[ranEf>0]    # remove any zero elements (typically these are unused of diagonal covariances)

  # Now parse the output and replace initial values with final estimates
  for(i in 1:nRan){# i=1
    # Identify all sections
    secs = data.frame(sec=ctl[grep("\\$", ctl)], ind=grep("\\$", ctl))
    secs$nextS = c(secs$ind[2:nrow(secs)], length(ctl))
    thisSec = grep(str, secs$sec)[i]
    startS = secs$ind[thisSec]
    endS = ifelse(secs$nextS[thisSec]==length(ctl), length(ctl),
                  secs$nextS[thisSec]-1)
    tmp = unlist(unPaste(ctl[startS:endS]))
    tmp = trim(sub("FIX", "", tmp))
    tmp = ifelse(regexpr("\\;", tmp) > 1, trim(substring(tmp, 1, regexpr("\\;", tmp)-1)), tmp)  # remove comments
    tmp = paste(tmp, collapse=" ")
     
    # How many values in the current line
    if(regexpr("BLOCK", tmp)>0) {
     dimen = as.numeric(substring(tmp,first=regexpr("\\(", tmp)+1, last=regexpr("\\)",tmp)-1))
     ival = sum(1:dimen)
     ivals[i] = ifelse(i==1, ival, ivals[i-1]+ival)
     ran[i] = ifelse(i==1, paste(repl, " BLOCK(", as.character(dimen), ") ", 
                                 paste(as.character(ranEf[1:ival]), collapse=" "),
                                 " FIX",
                                 sep=""),
                           paste(repl, " BLOCK(", as.character(dimen), ") ", 
                                 paste(as.character(ranEf[(ivals[i-1]+1):ivals[i]]), collapse=" "),
                                 " FIX",
                                 sep="")
                           )
    } else{
      tmp1 = trim(substring(tmp, first=regexpr(str, tmp)+6))
      ival = length(unlist(strsplit(tmp1, " +")))
      ivals[i] = ifelse(i==1, ival, ivals[i-1]+ival)
      ran[i] = ifelse(i==1, paste(repl, 
                                  paste(as.character(ranEf[1:ival]), collapse=" "),
                                  "FIX",
                                  sep=" "),
                            paste(repl, 
                                  paste(as.character(ranEf[(ivals[i-1]+1):ivals[i]]), collapse=" "),
                                  "FIX",
                                  sep=" ")
                            )
    } # else
  } # for
  return(ran)
} # makeRandom   

# ROXYGEN Documentation
#' Sample parameters across uncertainty
#' @param out output from nm.extract.xml
#' @return single draw from quasi-posterior parameter distribution
#' @export
#' @import Hmisc

uncertainParams = function(out){
  ## Take output from nm.extract and return sampled fixed and random effects 
  ## vectors based on the uncertainty matrix
  len = length(out$fixef)
  fixLen = length(out$fixef[[len]])
  sigLen = length(out$sigef[[len]])
  omLen = length(out$ranef[[len]])
  covMat = out$covariance[[len]]
  expected = c(out$fixef[[len]],out$sigef[[len]], out$ranef[[len]])
  vec = mvrnorm(n=1, mean=expected, Sigma=covMat)
  outParams=vector("list")
  outParams$fixed = vec[1:fixLen]
  outParams$sigma = vec[(fixLen+1):(fixLen+sigLen)]
  outParams$omega = vec[(fixLen+sigLen+1):length(vec)]
  return(outParams)
}

# ROXYGEN Documentation
#' Documentation stub
#' Create a control stream for simulation
#' @description Create a control stream for simulation with sampled TH/OM/SG estimates 
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param outputFile.extension file extension of the NONMEM output file. Defaults to ".lst"
#' @param modelFile.extension file extension of the NONMEM control stream. Defaults to ".mod"
#' @param seed seed number to go into \code{$SIM}
#' @param newDataFile if some else as the model data file should be used
#' @param uncertainty logical for simulation across uncertainty
#' @param sim to be described
#' @param table a character string with the required \code{$TABLE} statement for the simulation control stream
#' @return A control stream ready for execution written to file
#' #' @note THIS FUNCTION HAS NOT BEEN THOROUGHLY TESTED. USE WITH CAUTION AND ADAPT AS NEEDED ON THE PROJECT LEVEL.
#' @export
#' @import Hmisc

nm.create.control = function(run,
  path = getOption("nmDir"),
  outputFile.extension = ".out",
  modelFile.extension = ".ctl",
  seed = 1234567,
  newDataFile = NULL, ## by default the model data set will be loaded and used for simulation
  uncertainty = F,  ## Switch to determine whether sims are done with parameter uncertainty
  sim = NULL,
  table = NULL
)
{
  # Fetch the control stream
  out = nm.extract.xml(run = run, path = path)
  ctl = out$control.stream
  ctl = trim(ctl)
  
  # Create replacement sections
  if(uncertainty){
    parms = uncertainParams(out)
    fix = paste("$THETA", parms$fixed, "FIX")       # fixed effects
    omega = makeRandom("omega", ctl, parms$omega)   # Omega
    sigma = makeRandom("sigma", ctl, parms$sigma)   # Sigma
  } else{  
    fix = paste("$THETA", out$fixef[[length(out$fixef)]], "FIX")
    omega = makeRandom("omega", ctl, out$ranef[[length(out$ranef)]])
    sigma = makeRandom("sigma", ctl, out$sigef[[length(out$sigef)]])
  }
  
  # Remove sections that are either unnecessary or 
  # will be replaced with new code
  remSections=Cs(COV, EST, THETA, OMEGA, SIGMA)
  ctl = removeSection(remSections, ctl)[[1]]
  
  ## create the control stream
  ctl = c(ctl, fix, omega, sigma)

  # Add simulation statment
  if(is.null(sim)) sim = paste("$SIM ONLYSIM (", seed, ")", sep = "")
  ctl = c(ctl, sim)

  # Change the table reference or add table statment
  if(!is.null(table)){
    ctl = removeSection("TABLE",ctl)[[1]]
    ctl = c(ctl, table)
  } else {
    # if no change to the table statement, simply change the table references to add sim
    # to the output file names
    tmp = removeSection("TABLE", ctl)
    ctl = tmp[[1]]
    table = unlist(tmp[[2]])
    table = sub("\\.tab", "sim.tab", table, ignore.case=T)
    table = sub("\\.par", "sim.par", table, ignore.case=T)
    ctl = c(ctl, table)   
  }
  
  ## Insert reference to new dataset if needed
  if(!is.null(newDataFile)){
     dataInd = grep("\\$DATA", ctl)
     ignore = substring(ctl[dataInd], regexpr("IGNORE=", ctl[dataInd])+7)
     dataLn = paste("$DATA ", newDataFile, " IGNORE=", ignore, sep="")
     ctl[dataInd] = dataLn  
  }
  
  write(ctl, paste(path, paste(run, "sim", modelFile.extension, sep = ""), sep = "/"))
}

examples = F
if(examples)
{
  nm.create.control("run11", path = getOption("qpExampleDir"), table = "$TABLE PRED IPRED DV ONEHEADER NOPRINT FILE = sim.tab")
}