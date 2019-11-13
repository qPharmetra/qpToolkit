
# ROXYGEN Documentation
#' Create covariate plot and data
#' @description Create ETA vs covariates and parameter estimates vs covariates. Programmed with the intention to get between 95-100\% of report Appendices ready plots. If not, the data.frames on basis of which they were created are available as well to enable creating the desired graphs very easily.
#' @param run run rootname (e.g. \code{run1})
#' @param path directory where \code{run} resides
#' @param covlist a list with elements \code{cat} and \code{con} each representing a character vector with categorical and continuous covariates, respectively.
#' @param catcov.sep max number of unique values for a covariate to be assigned 'categorical'
#' @param parameters a character vector with parameter names to plot
#' @param aspect lattice banking aspect for rectangular or square plots. Defaults to 'fill'
#' @param pcx dot size
#' @param uniques set to TRUE to look at one value per ID only. defaults to F
#' @param which.list which element of the list in case multiple regression methods were used. Defaults to 1.
#' @param id.var character string denoting grouping variable ("ID")
#' @param iov inter-occasion variability flag (this argument is ignored currently)
#' @param eta.densities logical to determine of ETA density plots have to be generated
#' @param eta.skip character vector of ETAs that will be skipped 
#' @param shrinkage providing shrinkage values. This is currently ignored
#' @return A list with plots and properly sorted / molten data.frames for customized plots
#' @export
#' @importFrom Hmisc unPaste Cbind
#' @importFrom latticeExtra useOuterStrips
#' @importFrom metrumrg panel.densitystrip panel.stratify
#' @importFrom reshape2 melt
#' @importFrom stats lowess qnorm
#' @import lattice
#' @examples
#' args(nm.covplot)
#' nmcov = nm.covplot(path = getOption("qpExampleDir")
#' , run = "example2", parameters = c("CL","V1","Q","V2")
#' , covlist = list(con = "AGE", cat = "GNDR"))
#' 
#' #contents of the list
#' names(nmcov)
#' #[1] "covdata"        "eta.splom"      "eta.dens"       
#' # "conData.eta"    "catData.eta"    "conData.par"    "catData.par"   
#' #[8] "etaContVarPlot" "etaCatVarPlot"  "parContVarPlot" "parCatVarPlot" 
#' 
#' # let's look at the plots
#' nmcov$eta.dens()
#' # red line is the distribution predicted obo OMEGA estimate
#' # the grey polygon is the actual ETA distribution
#' 
#' ## elegant pairs plot
#' nmcov$eta.splom()
#' 
#' ## ETAs vs. Continuous Covariates
#' nmcov$etaContVarPlot()
#' 
#' ## ETAs vs. Categorical Covariates
#' nmcov$etaCatVarPlot()
#' 
#' ## parameters vs. Continuous Covariates
#' nmcov$parContVarPlot()
#' 
#' ## parameters vs. Categorical Covariates
#' nmcov$parCatVarPlot()
#' 
#' ## data frames required to replot the above are right there for convenience
#' head(nmcov$covdata) ## merged get.xpose.table and nm.params.table output!
#' head(nmcov$conData.eta)# essential: conVariable conValue variable value
#' head(nmcov$catData.eta)# essential: catVariable catValue variable value
#' head(nmcov$conData.par)# essential: conVariable conValue variable value
#' head(nmcov$catData.par)# essential: catVariable catValue variable value
#' 
#' #for continuous plots get kickstarted with:
#' # xyplot(value ~ conValue | casefold(variable, upper=TRUE) * conVariable
#' #  , data = conData, panel = panel.xyplot)
#' #for categorical plots:
#' # xyplot(value ~ as.factor(paste(catValue)) | variable * catVariable
#' #  , data = catData, panel = panel.bwplot, horizontal = FALSE)
#' 


nm.covplot = function(run = "run1", 
  path = getOption("nmDir"), 
  id.var = "ID",
  iov = FALSE,
  eta.densities = TRUE,
  eta.skip = NULL,
  shrinkage = FALSE,
  covlist = list(cat = c(NULL), con = c(NULL)),
  catcov.sep = 5, # max number of unique values for a covariate to be assigned 'categorical'
  parameters = NULL,
  aspect = "fill",
  pcx = 0.5,  # pointsize
  uniques = FALSE, ## set to TRUE to look at one value per ID only
  which.list = 1  ## which element of the list in case multiple regression methods were used
  )
{
#   require(Hmisc)
#   require(latticeExtra)
#   require(reshape)
#   
  id.var = casefold(id.var, upper = FALSE)
  
  ## reset everything
  parContVarPlot = parCatVarPlot = etaContVarPlot = etaCatVarPlot = eta.dens = eta.splom = NULL
  catData.par = conData.par = catData.eta = conData.par = NULL
  
  default.covlist = list(cat = c("sex","race"), con = c("age", "wt", "bmi", "ht", "crcl" ,"base"))
  if(!missing(covlist) & any(names(covlist) %nin% c('cat','con'))) stop("if specified argument covlist must have elements cat and con both as vectors")
  
  ## read NONMEM table output
  nmcov = get.xpose.tables(run = run, path = path) 
  names(nmcov) = casefold(names(nmcov), upper=FALSE)
  if(length(nmcov[, id.var]) == 0){
    message(paste(id.var, "does not exist in",run,". nm.covplot.procedure stopped"));return()
  }

  ## need to add a workaround for IOV in which case we do not want to drop OCCs within individual
  nmcov = nmcov[duplicated(nmcov[, id.var]) == FALSE, ]
  
  ## pull the ETAs from the NONMEM output table
  eta.names = names(nmcov)[grep("et", names(nmcov))]
  eta.list = eta.names[!is.na(extract.number(eta.names))]
  
  ## remove ETAs per instruction
  if(!is.null(eta.skip))
  {
    nok = eta.names %nin% casefold(eta.skip, upper = FALSE)
    eta.names = eta.names[nok]
    eta.list = eta.list[nok]
  }

  ## remove etas that are all zero (probably fixed)
  all.zero.etas = apply(nmcov, 2, function(x) all(x == 0))
  msel = substring(names(nmcov), 1, 2) == "et" 
  #if(length(which(msel==T))>0) nmcov = nmcov[,-which(msel)]

  # assing categorical in case of less than catcov.sep categories
  type = rep("continuous", ncol(nmcov))
  type[apply(nmcov, 2, lunique) < catcov.sep] = "categorical"

  ## add estimated parameter estimates to data frame
  thePars = nm.params.table(run, path = path)
  fixPars = as.numeric(thePars$Estimate)
  fixPars = fixPars[grepl("THETA",thePars$Parameter)]
  names(fixPars) = thePars$Parameter[grepl("THETA",thePars$Parameter)]
  #fixPars = unlist(fixPars[[which.list]])
  fixPars = vec2df(fixPars,nrow(nmcov))

  ranPars = as.numeric(thePars$Estimate)
  ranPars = ranPars[grepl("OMEGA",thePars$Parameter)]
  names(ranPars) = thePars$Parameter[grepl("OMEGA",thePars$Parameter)]
  #ranPars = unlist(ranPars[[which.list]])
    rp = ranPars
  names(ranPars) = gsub("ETA","OMEGA", names(ranPars))
  ranPars = vec2df(ranPars,nrow(nmcov))
  
  data = cbind(nmcov, fixPars, ranPars)
  #return(data)}

  ## find out which parameter in the data set is continuous
  ## and which is categorical
  
  if(FALSE)
  {
    data=nmcov
    data$sex = sample.by.id(data$id, c("F","M"))
    data$race = sample.by.id(data$id, c("Caucasian","Black","Asian", "Other"))
    data$crcl = rnorm.by.id(data$id, 0.6, 0.17)
    data$wt = sample.by.id(data$id, rpois(n=1000,lambda = 75))
    data$cl = data$cl * (1 + 0.02 * data$wt)
  }
  
  ## only keep the covariates that actually exist in the NONMEM output tables
  if(is.null(covlist$cat)) {
    cat.list = intersect(default.covlist$cat, names(data)) 
    } else {
      covlist$cat = casefold(covlist$cat, upper = FALSE) 
      cat.list = intersect(covlist$cat, names(data))
    }
  if(is.null(covlist$con)) {
    con.list = intersect(default.covlist$con, names(data))
    } else {
      covlist$con = casefold(covlist$con, upper = FALSE) 
      con.list = intersect(covlist$con, names(data))
    }
  #intersect(eta.list, names(data))
   
  eta.splom = function()
  {
    splom(data[, eta.list], 
     panel = function(x, y){
       	panel.splom(x, y, col = gray[7])
       	panel.lines(lowess(x,y), col = red[7], lwd = 2)
       	panel.abline(h = 0, v = 0, col = gray[5])
     },
     varnames = casefold(eta.list, upper = TRUE),
     pscales = 1,
     xlab="", ylab = "")
  }

 
  etas = reshape2::melt(data, measure.var = eta.list)
  data.omega.names = names(data)[grep("OMEGA", names(data))]
  data.omega.names = data.omega.names[apply(do.call("rbind", lapply(unPaste(data.omega.names, sep = "[.]"), extract.number)), 2, diff) == 0]
  om.index = floor(extract.number(data.omega.names))
  data.omega.names = data.omega.names[intersect(om.index,extract.number(eta.list))]
  omegas = reshape2::melt(data, measure.var = data.omega.names)
  omegas = omegas[, c('variable', 'value')]
  omegas$est.value = omegas$value
  omegas$variable = omegas$value = NULL
  etas = cbind(etas, omegas)
  etas$index = extract.number(etas$variable)
  ## head(etas)
  
  ## get shrinkage
  if(shrinkage)
  {
    shrink = invisible(try(get.shrinkage(run, path = path)$eta))
    if(class(shrink) == "try-error")
    {
      shrink = with(etas, reapply(Cbind(value,est.value)
                                  , INDEX = list(variable)
                                  , FUN = function(x) 1-sd(x)/sqrt(mean(attr(x, "other")))
      ))
      shrink = unique(shrink) * 100
      names(shrink) = eta.list
    } 
    names(shrink) = casefold(names(shrink), upper = FALSE)
    shrink = round(shrink[eta.names], 2)
    shrink = data.frame(variable = names(shrink), etashrink = shrink)
    etas = stableMerge(etas, shrink)
  }
  
  eta.dens = function()
  {
    xyplot(variable ~ value, #x=etas$value; y = etas$variable
		  data = etas,
		  tmp = etas,
		  aspect = 1.5,
      ylim = c(1,lunique(etas$index)) + c(-0.2, 0.7),
      xlim = max(abs(etas$value)) * c(-1.02,1.02),
      ed = eta.densities,
      panel = function(x, y, tmp, ed, ...)
      {                                             
        x = as.numeric(x)
        y = as.numeric(y)
        
        ## get the estimated eta values
        z = tapply(tmp$est.value, tmp$index, unique)
        zx = sapply(z, function(ooo)qnorm(seq(0.001,0.999, length = 512), 0, sqrt(ooo)))
        zz = zy = rep(seq(length(names(z))), ea = 512) 
        zy = zy + c(apply(zx, 2, function(ooo)unitDensity(ooo)$y)) * 0.5
        zx = c(zx)

       if(ed) metrumrg::panel.stratify(panel.levels = metrumrg::panel.densitystrip,  x = x, y = y, horizontal = TRUE, border = TRUE, col = gray[1], ...)

       xtpos = current.panel.limits()$xlim[1] + diff(current.panel.limits()$xlim)*0.1
       ytpos = current.panel.limits()$ylim[1] + diff(current.panel.limits()$ylim)*0.95
       
       if(F)
       {
         shrinkText = tapply(tmp$etashrink, tmp$index, mean)
         ltext(x = rep(xtpos,6), y = seq(length(names(z)))+length(names(z))/30, shrinkText, cex = 0.75)
         ltext(xtpos, ytpos, expression(paste(eta[sh]," (%)")), cex = 1)
       }
       
       panel.superpose(x=zx, y = zy, groups = zz, subscripts = TRUE,..., col = red[3], type = "l")
       panel.segments(x1=0, x2=0, y1=1, y2=max(etas$index)+1.2, col = gray[3])
      },
      ylab = "",
      xlab = "deviation",
      scales = list(y = list(at=seq(lunique(levels(etas$variable)))
                             , labels = casefold(levels(etas$variable),upper=TRUE))),
      as.table = TRUE
    )
  }

  ## prepare continuous covariates
  if(length(con.list)>0)
  {
  conData = reshape2::melt(data, measure.vars = con.list)
  names(conData)[names(conData) %in% c('variable','value')] = c('conVariable', 'conValue')
  conData.eta = reshape2::melt(conData, measure.vars = eta.list)

  etaContVarPlot = function()
  { 
    useOuterStrips(
      xyplot(value ~ conValue | casefold(variable, upper=TRUE) * conVariable,
     data = conData.eta,
    pcx = pcx,
    panel = function(x,y, ..., pcx)
    {
      panel.xyplot(x,y, ..., col = gray[10], cex = pcx
                   , type = c("p"))#,"r"), col.line = blue[5], lwd = 2)
      llines(lowess(x, y), col = red[7], lwd = 2)
      panel.abline(h=0, col = gray[5], lwd = 1)
    },
    scales = list(relation = "free"),
    #between = list(x = 0.5, y = 0.5),
    aspect = aspect,
    xlab = "continuous covariate value",
    ylab = "deviation by ID",
    ))
  }
  }
  
  ## prepare categorical covariates
  if(length(cat.list)>0)
  {
  catData = reshape2::melt(data, measure.vars = cat.list)
  names(catData)[names(catData) %in% c('variable','value')] = c('catVariable', 'catValue')
  catData.eta = reshape2::melt(catData, measure.vars = eta.list)

  etaCatVarPlot = function()
  { 
  useOuterStrips(
    xyplot(value ~ as.factor(paste(catValue)) | casefold(variable, upper=TRUE) * catVariable,
    data = catData.eta,
    horizontal = FALSE,
    panel =  function(x,y,...)
    {
      panel.bwplot(x,y,...)
      panel.abline(h = 0, col = gray[5], lwd = 1)
      yy = tapply(y,x,mean)
      llines(as.numeric(as.factor(names(yy))), yy, col = red[7])
    },
    aspect = aspect,
    xlab = "categorical covariate value",
     ylab = "deviation by ID",
    scales = list(relation = "free")
    ) 
  )
  }
  }
  
  ## prepare continuous parameters
  if(!is.null(parameters) & length(con.list) > 0)            #  parameters=c("tvlamb","disp")
  {
  conData = reshape2::melt(data, measure.vars = con.list)
  names(conData)[names(conData) %in% c('variable','value')] = c('conVariable', 'conValue')
  conData.par = reshape2::melt(conData, measure.vars = casefold(parameters,upper = FALSE))

  parContVarPlot = function()
  { 
    useOuterStrips(
      xyplot(value ~ conValue | casefold(variable, upper=TRUE) * conVariable,
    data = conData.par,
    pcx = pcx,
    panel = function(x,y, ..., pcx)
    {
      panel.xyplot(x,y, ..., col = gray[10], cex = pcx
                   , type = c("p"))#,"r"), col.line = blue[5], lwd = 2)
      llines(lowess(x, y), col = red[7], lwd = 2)
    },
    scales = list(x = list(relation = "free"), y = list(relation = "free")),
    between = list(x = 0.5, y = 0.5),
    aspect = aspect,
    xlab = "continuous covariate value",
    ylab = "Parameter Value",
    )
    )
  }
  }
  
  ## prepare categorical parameters
  if(!is.null(parameters) & length(cat.list)>0)
  {
  catData = reshape2::melt(data, measure.vars = cat.list)
  names(catData)[names(catData) %in% c('variable','value')] = c('catVariable', 'catValue')
  catData.par = reshape2::melt(catData, measure.vars =  casefold(parameters,upper = FALSE))

  parCatVarPlot = function()
  { 
    useOuterStrips(
      xyplot(value ~ as.factor(paste(catValue)) | casefold(variable, upper=TRUE) * catVariable,
    data = catData.par,
    horizontal = FALSE,
    panel =  function(x,y,...)
    {
      panel.bwplot(x,y,...)
      yy = tapply(y,x,mean)
      llines(as.numeric(as.factor(names(yy))), yy, col = red[7])
    },
    scales = list(x = list(relation = "free"), y = list(relation = "free")),
    aspect = aspect,
    xlab = "categorical covariate value",
    ylab = "Parameter Value"
    ) 
    )
  }
  }
  cat("processed NONMEM output and covariate data and created covariate plots of"
      , lunique(data[, id.var]), "subjects")
  return(list(  covdata = data, 
                eta.splom = eta.splom, 
                eta.dens = eta.dens, 
                conData.eta = conData.eta,
                catData.eta = catData.eta,
                conData.par = conData.par,
                catData.par = catData.par,
                etaContVarPlot = etaContVarPlot,
                etaCatVarPlot = etaCatVarPlot, 
                parContVarPlot = parContVarPlot,
                parCatVarPlot = parCatVarPlot,
                etas = etas)
  )
}



