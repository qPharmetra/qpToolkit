# ROXYGEN Documentation
#' Create covariate plot and data
#' @param path directory where vpc_results and vpctab files reside
#' @param result filename of the 'vpc_results.csv" file
#' @param tab filename of the 'vpctab' file
#' @param PI.limits empty
#' @param subset empty
#' @param output empty
#' @param relabel.strata empty
#' @param subtitle empty
#' @param logY empty
#' @param logX empty
#' @param xLabel empty
#' @param yLabel empty
#' @param cex.label empty
#' @param cex.strip.labels empty
#' @param xLimits empty
#' @param yLimits empty
#' @param aspect empty
#' @param scales empty
#' @param xscale.components empty
#' @param yscale.components empty
#' @param layout empty
#' @param showPredAs empty
#' @param showObsDots empty
#' @param showObsLines empty
#' @param lineType = "l",empty
#' @param central.lwd empty
#' @param obscol.dot empty 
#' @param obscex.dot empty
#' @param obspch.dot empty
#' @param obscol.line empty
#' @param predcol.central empty
#' @param predcol.outer empty
#' @param predcol.area.central empty
#' @param predcol.area.outer empty
#' @param predcol.area empty
#' @return a lattice VPC graph
#' @note This function should not be used as it was marked for future deprecation. qPharmetra's default VPC plot tool is xpose.VPC
#' @export
#' @import lattice
#' @importFrom nlme getCovariateFormula
#' @importFrom stats as.formula
nm.vpcplot = function(path, 
         result = "vpc_results.csv", 
         tab = dir(path=path,pattern = "^vpctab")[1],
         PI.limits = c(0.025,0.975),
         subset = NULL,
         output = FALSE,
         relabel.strata = NULL,
         subtitle = "",
         logY = FALSE,
         logX = FALSE,
         xLabel = NULL,
         yLabel = NULL,
         cex.label = 1.25,
         cex.strip.labels = 1,
         xLimits = NULL,
         yLimits = NULL,
         aspect = 1,
         scales = NULL,
         xscale.components = NULL,
         yscale.components = NULL,
         layout,
         showPredAs = "area",
         showObsDots = TRUE,
         showObsLines = TRUE,
         lineType = "l",
         central.lwd = 1.5,
         obscol.dot = gray[8], 
         obscex.dot = 0.5, 
         obspch.dot = 1,
         obscol.line = gray[10],
         predcol.central = red[6],
         predcol.outer = blue[7],
         predcol.area.central = red[2],
         predcol.area.outer = blue[1],
         predcol.area = blue[1]
)
{
  #require(xpose4)
  
  rootName = unlist(unPaste(path)[(length(unPaste(path)))])
  
  ## turn individual layout options into a list
  col.scheme = list(obs = list(dot = obscol.dot, line = obscol.line), 
                    pred = list(central = predcol.central, outer = predcol.outer, 
                                area = predcol.area,
                                area.central = predcol.area.central,
                                area.outer = predcol.area.outer)) 
  
  ## parse out the vpc results into predicted (sim) and observed (obs)
  vpc = read.vpc(path = path, result = result, tab=tab)
  sim = vpc$vpc
  obs = vpc$tab
  responseVar = vpc$vpc$dv.var
  xCov = vpc$vpc$idv.var
  
  ## relabel strata names
  ## check if relabeling request can be done
  original.strata.names = vpc$vpc$strata.names
  if(!is.null(relabel.strata) & {length(relabel.strata$old) != length(vpc$vpc$strata.names) | 
   length(relabel.strata$new) != length(vpc$vpc$strata.names)})
  {
    message("If argument relabel.strata is specified it needs to have the same
            length as the number of start in th VPC. VPC plot routine aborted.")
    return()
  }
  stop('this function requires review')
  ## CHECK exchange.values not defined
  exchange.values <- function(x)x
  if(!is.null(relabel.strata) & length(relabel.strata$old) == length(vpc$vpc$strata.names))
  {
    vpc$vpc$strata.names = exchange.values(vpc$vpc$strata.names, relabel.strata$old, relabel.strata$new)
  }


  ## pull out the correct statistics
  msel.ci = grep(".CI.for.", names(sim$result.tables[[1]]))
  suffix =  unique(sub("([0-9]+).CI.for(.*)","\\1",names(sim$result.tables[[1]])[msel.ci]))               
  msel.ci = grep(paste(suffix,".CI.for.",sep=""), names(sim$result.tables[[1]]))
  msel.pi = sub("([0-9]+).CI.for(.*)","\\2",names(sim$result.tables[[1]]))
  msel.pi = sub("to", "", msel.pi)
  msel.pi = sub("from", "", msel.pi)
  msel.pi = substring(msel.pi, 2, (nchar(msel.pi)-1))
  msel.pi = which(msel.pi %in% as.character(c(PI.limits*100,50)))
  ci.names = names(sim$result.tables[[1]])[intersect(msel.ci,msel.pi)]

  real.names = names(sim$result.tables[[1]])[grep("real", names(sim$result.tables[[1]]))]
  real.names2 = sub("([0-9]+).real","\\1",real.names)
  real.names2 = real.names[real.names2 %in% as.character(c(PI.limits*100,50))]
  real.names = names(sim$result.tables[[1]])[names(sim$result.tables[[1]]) %in% real.names2]

  sim.names = names(sim$result.tables[[1]])[grep("sim", names(sim$result.tables[[1]]))]
  sim.names2 = sub("([0-9]+).sim","\\1",sim.names)
  sim.names2 = sim.names[sim.names2 %in% as.character(c(PI.limits*100,50))]
  pred.names = names(sim$result.tables[[1]])[names(sim$result.tables[[1]]) %in% sim.names2]
  
  ## organize predicted results
  names(sim$result.tables[[1]])
  sim$result.tables = sim$result.tables[unlist(lapply(sim$result.tables, function(x) 
  class(x)  == "data.frame"))]
  SIM = lapply(sim$result.tables
             , function(x){
                x = x[, c("lower","upper",pred.names, real.names, ci.names)]
                names(x)= c('lower.xcov','upper.xcov', 'ypred.cen', 'ypred.lo',
                 'ypred.hi', 'yobs.cen', 'yobs.lo', 'yobs.hi'
                             , 'ypred.cen.dn', 'ypred.cen.up'
                             , 'ypred.lo.dn', 'ypred.lo.up'
                             , 'ypred.hi.dn', 'ypred.hi.up')
                return(x)
             }
  )

  SIM = lapply(SIM, function(x) {x$xcov = if(all(is.na(x$lower.xcov))) x$upper.xcov else{
  apply(x[,c('lower.xcov','upper.xcov')],1,mean)};  return(x)})#x = VPCSIM[[2]]

  ## apply check for log
  if(logY)
  SIM = lapply(SIM, function(x){ #x = SIM[[4]]
    xx = x[, c('ypred.lo', 'ypred.cen', 'ypred.hi', 'yobs.lo', 'yobs.cen', 'yobs.hi')]
    msel = which(xx<=0)
    xx = apply(xx, 2, function(y){
      y[y<=0] = rep(min(y[y>0]) * 0.01, sum(y<=0)); return(y)}
    )
    x[,c('ypred.lo', 'ypred.cen', 'ypred.hi', 'yobs.lo', 'yobs.cen', 'yobs.hi')] = 
      if(nrow(x) > 1){
        xx[,c('ypred.lo', 'ypred.cen', 'ypred.hi', 'yobs.lo', 'yobs.cen', 'yobs.hi')]
        } else {
         matrix(xx[c('ypred.lo', 'ypred.cen', 'ypred.hi', 'yobs.lo', 'yobs.cen', 'yobs.hi')],nrow=1)
       }
       
    return(x)
  })

#   if(logX == T & any(obs[,xCov] <= 0)) 
#   {
#     message("cannot do a log on x-covariate values less or equal to 0. 
#             nm.vpcplot procedure aborted.")
#     return()
#   }

## bind stratified results into singe data frame
c.or.unlist = function(x) if(is.list(x)) unlist(x) else c(x) 
## sometimes the strata appear as list and sometimes as a matrix
VPC = do.call("rbind", SIM)

if(logX & any(obs[,xCov] <= 0)) 
{
  obs[,xCov][obs[,xCov] <= 0] = min(obs[,xCov][obs[,xCov] > 0], na.rm=TRUE)
  VPC$xcov[VPC$xcov == 0] = min(obs[,xCov])
}


VPC$strata = c.or.unlist(apply(rbind(vpc$vpc$strata.names, unlist(lapply(SIM, nrow))),
                               2, function(x) rep(x[1],x[2])))
VPC$strata_no = c.or.unlist(apply(rbind(seq(along=original.strata.names), 
                                        unlist(lapply(SIM, nrow))),2, function(x) rep(x[1],x[2])))
names(VPC)[names(VPC) == 'xcov'] = xCov
if(is.null(relabel.strata)) VPC$strata = gsub(" ", "", VPC$strata)

## apply a subset, if applicable
if(!is.null(subset)){
  ## check if subset is OK, can be subset on strata and on idv
  checkVars = all.vars(getCovariateFormula(paste("a~",gsub(" ","", subset)))[[2]])
  if(all(checkVars %in% names(obs))){ #subset = "DOSE >120 & TAD<336"
    msel.vpc = eval(parse(text = subset), obs)
    obs = obs[msel.vpc, ]
    VPC = VPC[VPC$strata_no %in% unique(obs$strata_no), ]
    unique(VPC$strata)
  } else {
    message(
      "at least 1 argument in subset does not match with column names in the VPC data set.")
  }
}

## create the correct plotting formula
myForm = as.formula(paste("Cbind(ypred.cen, ypred.lo, ypred.hi, ypred.cen.dn, ypred.cen.up, ypred.lo.dn, ypred.lo.up, ypred.hi.dn, ypred.hi.up) ~"
                          , xCov
                          , "| format(strata)"
                          , sep = " "))
if(lunique(VPC$strata) == 1) myForm = 
  as.formula(paste("Cbind(ypred.cen, ypred.lo, ypred.hi) ~", xCov, sep = " "))

## x and y labels    #xLabel = yLabel = NULL
xLabel = if(is.null(xLabel)) xCov else xLabel
yLabel = if(is.null(yLabel)) responseVar else yLabel   

## define axes limits
yLim = range(c(VPC[, c('ypred.cen', 'ypred.lo', 'ypred.hi')], obs$DV))
yLim = yLim + c(0, 0.05 * diff(yLim))

# define layout if not specified
len = lunique(VPC$strata)
if(missing(layout)) layout = c(round(sqrt(len)), round(sqrt(len+0.49)))

# no xLimits or yLimits set
if(is.null(xLimits) & is.null(yLimits)){
  xLimits = range(VPC[, xCov])
  yLimits = yLim
} else 
  
  if(is.null(xLimits) & !is.null(yLimits)) {   # Only yLimits set
    xLimits = range(VPC[, xCov])
    yLimits = yLimits + c(0, 0.05 * diff(yLimits))  
  } else 
    
    if(!is.null(xLimits) & is.null(yLimits)){ # Only xLimits set
      theYs = c(as.vector(as.matrix(VPC[VPC[, xCov]>=xLimits[1]&VPC[, xCov]<=xLimits[2], 
                                        c('ypred.cen', 'ypred.lo', 'ypred.hi')])),
                obs$DV)
      ylm = range(theYs)
      yLimits = ylm + c(0, 0.05 * diff(ylm))
    } else {    # xLimits and yLimits set
      theYs = c(as.vector(as.matrix(VPC[VPC[, xCov]>=xLimits[1]&VPC[, xCov]<=xLimits[2], 
                                        c('ypred.cen', 'ypred.lo', 'ypred.hi')])),
                obs$DV)
      theYs = theYs[theYs>=yLimits[1] & theYs<=yLimits[2]]
      ylm = range(theYs)
      yLimits = ylm + c(0, 0.05 * diff(ylm))
    }  

myScales = list(x = list(relation = "free"))
myComponents.y = yscale.components.default
myComponents.x = xscale.components.default

if(logY){
  myScales = list(y = list(log = 10), x = list(relation = "free"))
  myComponents.y = yscale.components.log10
}
if(logX){
  myScales = list(x = list(log = 10, relation = "free"))
  myComponents.x = xscale.components.log10
}
if(logY & logX ){
  myScales = list(y = list(log = 10), x = list(log = 10, relation = "free"))
  myComponents.x = xscale.components.log10
  myComponents.y = yscale.components.log10
}  

if(!is.null(scales)) myScales = scales
if(!is.null(xscale.components)) myComponents.x = xscale.components
if(!is.null(yscale.components)) myComponents.y = yscale.components



## do the renaming of start if applicable
VPC$strata = exchange.values(VPC$strata, original.strata.names, vpc$vpc$strata.names)

## final plotting routine; separate calls for logged and non-logged versions
vpcdump = VPC
VPC = 
  xyplot(myForm, 
         data = VPC,
         main = paste(rootName, paste((1-PI.limits[1]*2)*100,"% CI",sep=""), subtitle),
         panel = panel.nonmem.vpc,
         lineType = lineType,
         central.lwd = central.lwd,
         ylim = yLimits,        
         xlim = xLimits,
         ylab = list(yLabel, cex = cex.label),
         xlab = list(xLabel, cex = cex.label),
         OBS = obs,
         par.strip.text = list(cex = cex.strip.labels),
         vpc = VPC,
         logY = logY,
         logX = logX,
         xCov = xCov,
         showPredAs = showPredAs,
         showObsDots = showObsDots,
         showObsLines = showObsLines,
         col.scheme = col.scheme,
         obscex.dot = obscex.dot, 
         obspch.dot = obspch.dot,
         between = list(x = 0.5, y = 0.5),
         scales = myScales,
         yscale.components = myComponents.y,
         xscale.components = myComponents.x,
         aspect = aspect,
         layout = layout    
  ) 

output.results = "VPC done"
if(output) output.results = list(vpc = vpcdump, obs = obs)
else return(VPC)

}


if(F)
{
  nm.vpcplot(file.path(nmDir, "vpc2169_all"), logY = TRUE)
  ## note the plotting is done using panel.nonmem.vpc inside the function
}
