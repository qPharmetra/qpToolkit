# name:     nm.extract.xml
# purpose:  extracts all output embedded in the XML file of a NONMEM run
# input:    run and number (character)
# output:   list with all NONMEM output
#

# ROXYGEN Documentation
#' NONMEM output extraction from XML output
#' @description Extracts all output embedded in the XML file of a NONMEM run
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param filename with full path of the XML file to be parsed. This is automatically created using qP workflow standards but can be specified as well
#' @param digits number of significant digits
#' @param digits.se number of significant digits in SE (when point estimates = T)
#' @param digits.cv number of significant digits in CV\% (when point.estimates = T)
#' @param digits.est number of significant digits in parameter estimates (when point.estimates = T)
#' @param xml.extension extension of the xml file. Defaults to ".xml"
#' @param zip.extension extension of the zip program. Defaults to ".7z"
#' @param zip.program full path to the executable of the zip program (7zip or winzip). Defauls to "c:/progra~1/7-zip/7z"
#' @param remove.obsolete  logical (F) defining if non-estimated parameters should be dropped for the parameter table.
#' @param na.value what to do with an NA value in the parameter table. Defaults to "n.d."
#' @param control_stream logical (F) defining if the control stream should be included as well.
#' @param get.xpose.tables logical (T) defining if the xpose tables should be integrated in the result as a single data frame using the function \code{get.xpose.tables}
#' @param quiet if TRUE (default) silences any message returned by the function and subfunctions.
#' @return An extensive list with all elements extracted from the XML output
#' @export
#' @import XML
#' @note Works for NM7.2 (up to 9 OMEGAs) and NM7.3 (bug free)
#' @examples
#' tmp = nm.extract.xml("example2", path = getOption("qpExampleDir"))
#' names(tmp)
#' tmp$final_objective_function
#' tmp$table
#' tmp$monitor
#' tmp = nm.extract.xml("example2", path = getOption("qpExampleDir"))


nm.extract.xml = function(
  run = "run1",
  path = getOption("nmDir"),
  filename = paste(path, run, paste(run, xml.extension,sep=""),sep="/"),
  digits = 3,
  digits.se = digits,
  digits.cv = digits,
  digits.est = digits,
  xml.extension = ".xml",
  zip.extension = ".7z",
  zip.program = "c:/progra~1/7-zip/7z",
  remove.obsolete = FALSE,
  na.value = "n.d.",
  control_stream = FALSE,
  get.xpose.tables = TRUE,
  quiet = TRUE)
{
  #xml.extension = ".xml";zip.extension = ".7z";remove.obsolete = F;na.value = "n.d.";control_stream = F;  path = paste(getwd(),"NONMEM", run, sep = "/")
  #digits = 4;digits.se = digits;digits.cv = digits;digits.est = digits;filename = paste(path,run,paste(run, xml.extension,sep=""),sep="/")
  #require(XML)

  fz = paste(filename, zip.extension, sep = "")
  ## unzip the xml file in case it is required
  if(file.exists(fz)&file.exists(filename)) file.remove(filename)
  if(file.exists(fz)) nm.unzip(run = run
             , path = file.path(path,run)
             , zip.extension = zip.extension
             , extension = xml.extension
             , zip.program = zip.program
             , quiet = quiet
    )

  top = xmlRoot(xmlTreeParse(filename))# names(top)
  TMP = top[["nonmem"]][["problem"]]
  TMP = TMP[grepl("estimation", names(TMP))]
  len = length(TMP)
  ## get estimation titles
  est.methods = sapply(1:length(TMP), function(x,TMP)
    xmlValue(TMP[x][["estimation"]][["estimation_title"]]), TMP = TMP)
  names(TMP) = est.methods

  if(control_stream) return(control_stream = xmlValue(top[["control_stream"]]))

  nmext = lapply(TMP, function(tmp,
                               remove.obsolete
                               , digits.est
                               , digits.cv
                               , digits.se
                               , na.value
                               , quiet) #tmp=TMP[[1]]
  {
    estimation.method = xmlValue(tmp[["estimation_method"]])
    if(!quiet) cat("extracting:", estimation.method,"\n")

    names(tmp)
    covInfo = lapply(asXMLNode(tmp[["covariance"]])$children, xml.Extract.NM.Matrix)
    corInfo = lapply(asXMLNode(tmp[["correlation"]])$children, xml.Extract.NM.Matrix)
    covariance = make.symmetric(unlist(covInfo))#sqrt(diag(fullCovMatrix))
    correlation = make.symmetric(unlist(corInfo))
    covNames = if(length(tmp[["covariance"]])>0)
      names(covInfo[length(covInfo)][[1]]) else NULL
    dimnames(covariance) = dimnames(correlation) = list(covNames,covNames)
    fixed = xml.Extract.NM.Value(asXMLNode(tmp[["theta"]]), what = "theta")
    fixedse = if(length(tmp[["covariance"]])>0)
      xml.Extract.NM.Value(asXMLNode(tmp[["thetase"]]), what = "TH.SE") else NULL
    sigma = lapply(asXMLNode(tmp[["sigma"]])$children, xml.Extract.NM.Matrix)
    sigma = unlist(sigma)
    sigmase = lapply(asXMLNode(tmp[["sigmase"]])$children, xml.Extract.NM.Matrix)
    sigmase = unlist(sigmase)
    if(!is.null(names(sigma))&!is.null(sigma)) names(sigma) = names(sigmase) = covNames[grep("SIGMA", covNames)]
    if(is.null(names(sigma))&!is.null(sigma)) names(sigma) = rep("SIGMA(1,1)", length(sigma))
    omega = lapply(asXMLNode(tmp[["omega"]])$children, xml.Extract.NM.Matrix)
    omega = unlist(omega)
    omegase = lapply(asXMLNode(tmp[["omegase"]])$children, xml.Extract.NM.Matrix)
    omegase = unlist(omegase)
    lom = length(omega); .nth <- floor((sqrt(8*lom + 1) - 1) / 2)
    omegaNames = paste("OMEGA(",rep(1:.nth,1:.nth),",",
                       unlist(sapply(1:.nth, function(x) seq(x))),")", sep = "")
    if(length(omega)>0){
      names(omega) = omegaNames
      omega = convert.omega(omega)
    }
    if(length(omegase)>0){
      names(omegase) = omegaNames
      omegase = convert.omega(omegase)
    }
    lsi = length(sigma); .nth <- floor((sqrt(8*lsi + 1) - 1) / 2)
    sigmaNames = paste("SIGMA(",rep(1:.nth,1:.nth),",",
                       unlist(sapply(1:.nth, function(x) seq(x))),")", sep = "")
    if(length(sigma)>0){
      names(sigma) = sigmaNames
      sigma = convert.sigma(sigma)
    }
    if(length(sigmase)>0){
      names(sigmase) = sigmaNames
      sigmase = convert.sigma(sigmase)
    }

    names(tmp)

    if(any(names(tmp) %in% "etabar")){ ## test if etabar is there (not for MCMC)
      popNames = as.character(unlist(lapply(asXMLNode(tmp[["etabar"]])$children,
                                            function(x) xmlAttrs(x))))
      etabar = lapply(asXMLNode(tmp[["etabar"]])$children, xml.Extract.NM.Value, what="")
      etabarse = lapply(asXMLNode(tmp[["etabarse"]])$children, xml.Extract.NM.Value, what="")
      etabarpval = if(any(names(tmp) %in% "etabarpval")){  ## we don not have etabarp in case of $MIX
        lapply(asXMLNode(tmp[["etabarpval"]])$children, xml.Extract.NM.Value, what="")
      } else NULL
      etashrink = lapply(asXMLNode(tmp[["etashrink"]])$children, xml.Extract.NM.Value, what="")
      epsshrink = lapply(asXMLNode(tmp[["epsshrink"]])$children, xml.Extract.NM.Value, what="")
      names(etabar) = names(etabarse) = names(etashrink) = names(epsshrink) = popNames
      if(!is.null(etabarpval)) names(etabarpval) = popNames
    } else
      etabar = etabarse = etabarpval = etashrink = epsshrink = NULL

    eigenvalues = as.vector(unlist(lapply(asXMLNode(tmp[["eigenvalues"]])$children,
                                          xml.Extract.NM.Value, what="")), "numeric")

    parallel_est = if(!is.null(tmp[["parallel_est"]])) xmlAttrs(tmp[["parallel_est"]]) else NULL
    monitor = unlist(lapply(tmp[["monitor"]]$children, xml.Extract.NM.Value))
    names(monitor) = unlist(lapply(tmp[["monitor"]]$children, function(x) xmlGetAttr(x, "iteration")))
    termination_status = xmlValue(tmp[["termination_status"]])
    termination_information = xmlValue(tmp[["termination_information"]])
    estimation_elapsed_time = as.numeric(xmlValue(tmp[["estimation_elapsed_time"]]))
    covariance_elapsed_time =  as.numeric(xmlValue(tmp[["covariance_elapsed_time"]]))
    final_objective_function = as.numeric(xmlValue(tmp[["final_objective_function"]]))
    final_objective_function_text = xmlValue(tmp[["final_objective_function_text"]])
    final_objective_function_std = if(estimation.method == "mcmc")
      as.numeric(xmlValue(tmp[["final_objective_function_std"]])) else NULL

    ## make parameter table
    point.estimates = c(unlist(fixed),unlist(omega),unlist(sigma))
    se.estimates = c(unlist(fixedse),unlist(omegase),unlist(sigmase))
    if(is.null(se.estimates)) se.estimates = rep(NA, length(point.estimates))
    cvperc = as.numeric(100*se.estimates / point.estimates)
    cvperc = signif(cvperc, digits.cv)
    msel = is.nan(cvperc)
    cvperc[msel] = rep(na.value, sum(is.nan(cvperc)))

    table = if(!is.null(point.estimates))
    {
      nm.params.table(run = run, path = path)
    } else
      NULL


    if(remove.obsolete & !is.null(table)) table = table[!msel, ]
    if(!is.null(table)) row.names(table) = 1:nrow(table)

    return(
      list(fixed=fixed,fixedse=fixedse,omega=omega,omegase=omegase,sigma=sigma,sigmase=sigmase,
           covariance=covariance,correlation=correlation, table=table,
           eigenvalues=eigenvalues,etabar=etabar,etabarse=etabarse,etabarpval=etabarpval,
           etashrink = etashrink, epsshrink = epsshrink,estimation.method=estimation.method,
           parallel_est=parallel_est,monitor=monitor,termination_status=termination_status,
           termination_information=termination_information,estimation_elapsed_time=estimation_elapsed_time,
           covariance_elapsed_time=covariance_elapsed_time,
           final_objective_function=final_objective_function,
           final_objective_function_text=final_objective_function_text,
           final_objective_function_std=final_objective_function_std)
    )
  }## end lapply(TMP)
  , remove.obsolete = remove.obsolete
  , na.value = na.value
  , digits.est = digits.est
  , digits.cv = digits.cv
  , digits.se = digits.se
  , quiet = quiet
  )
  ## re-order the list obtained by item instead of by estimation method
  nmext = shuffle.list(nmext)

  nmTab = NULL
  xposeTablesExist = length(
    dir(path)[grepl("(sd|pa|ca|co)tab[0-9]+",dir(path))]
  ) > 0
  if(get.xpose.tables & xposeTablesExist)
    nmTab = get.xpose.tables(run = run, path=path)
  nmext$XPtable =  nmTab

  ## remove XML file that was unzipped
  if(file.exists(fz)) file.remove(filename)

  cat("extraction of",paste(run,xml.extension,sep = ""),"complete.\n")
  return(nmext)
} ## end nm.extract.xml




