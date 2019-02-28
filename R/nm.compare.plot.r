# ROXYGEN Documentation
#' Create a runrecord like GOF plot of selected runs
#' @param runs character vector with rootname runs like \code{c("run1","run2")}
#' @param path directory where \code{runs} reside
#' @param keep.cols needs at the minimum \code{Cs(ID,DV,CWRES,PRED,IPRED,TIME,EVID)} but can be altere to match other data columns
#' @param alias a named list to rename DV and TIME variables, in case in \code{$DATA} they have been specified as \code{...=DV} and/or \code{...=TIME}
#' @param dot.size scalar for GOF plot dots
#' @param text.size scalar for text size in strip labels
#' @param log a logical indicating if the GOF plots need to be logged
#' @return A plot
#' @export
#' @import Hmisc
#' @importFrom latticeExtra useOuterStrips ggplot2like
#' @importFrom reshape2 melt
#' @examples
#' nm.compare.plot(runs = c("example1","example2")
#' , path =getOption("qpExampleDir")
#' , alias = list(DV = "CONC",TIME = "TIME")
#' , keep.cols = Cs(ID,DV,CWRES,PRED,IPRED,TIME,EVID)
#' , text.size = 0.6
#' )


nm.compare.plot = function(runs
                           , path = getOption("nmDir")
                           , keep.cols = Cs(ID,DV,CWRES,PRED,IPRED,TIME,EVID)
                           , alias = list(DV = "CONC", TIME = "TAFD") ## in case of $INPUT CONC=DV TAFD=TIME etc...
                           , dot.size = 1 ## relative dot size
                           , text.size = 1 ## relative fontsize
                           , log = FALSE
                           )
{
  myOrder = runs
  
  out = lapply(as.list(runs), function(x, mypath) get.xpose.tables(run = x, path = mypath), mypath = path)
  names(out) = runs
  ## check for aliases
  if(any(unlist(lapply(out, function(x, keep.cols) length(setdiff(keep.cols, names(x)))>0, keep.cols = keep.cols))))
  {
    out = lapply(out, function(x, alias) 
    {
      if(length(x[, alias$DV])>0) x$DV = x[, alias$DV]
      if(length(x[, alias$TIME])>0&length(x$TIME)==0) x$TIME = x[, alias$TIME]
      return(x)
    }, alias = alias)
  }
    
  out = lapply(seq(length(runs)), function(x, out, keep.cols){
    X = out[[x]][, keep.cols]
    X$run = names(out)[x]
    return(X)
    }, out = out, keep.cols = keep.cols)
  out = do.call("rbind", out)
  out = out[out$EVID == 0, ]
  
  if(log){
    out$PRED = log10(out$PRED)
    out$TIME = log10(out$TIME)
    out$IPRED = log10(out$IPRED)
    out$DV = log10(out$DV)
  }
  
  ## melt elements
  molten = reshape2::melt(out, measure.vars = Cs(TIME,PRED,IPRED))
  names(molten)[names(molten) %in% Cs(variable,value)] = Cs(xVariable, xValue)
  molten = reshape2::melt(molten, measure.vars = Cs(DV,CWRES))
  molten$group = paste(molten$variable,molten$xVariable,sep="~")
  molten = molten[molten$group %nin% c("DV~TIME","CWRES~IPRED"), ]
  molten$variable = as.character(molten$variable )
  tail(molten)
  
  ## add model info
  ofv = as.vector(sapply(runs, function(x, mypath) get.ofv(x,path = mypath)[1], mypath = path))
  cat(ofv)
  tmp = molten[duplicated(molten$run) == FALSE,]
  tmp = rbind(tmp,tmp)
  tmp$xValue = rep(0:1, ea = length(runs))
  tmp$value = rep(0:1, ea = length(runs))
  tmp$group = "run.info"
  tmp$variable = ofv
  class(molten$xValue)
  
  molten = rbind(molten, tmp)
  molten$variable[molten$group == "main.info"]
  molten$run = factor(molten$run, levels = myOrder)
  
  size = length(runs) 
  myXscale = if(log) xscale.components.log10 else xscale.components.default
  #myYscale = if(log) yscale.components.log10 else yscale.components.default
  
  latticeExtra::useOuterStrips(
    xyplot(value ~ xValue | group * run
    , data = molten
    , aspect = 1
    , DATA = molten
    , scales = list(relation = "free", cex = 1/sqrt(size))
    , par.strip.text = list(cex = 2/sqrt(size))
    , dotSize = dot.size/sqrt(size) 
    , textSize = 2*text.size/sqrt(size) 
    , panel = function(x,y, subscripts, DATA, dotSize, textSize, ...)
    {
      func = unique(DATA$group[subscripts])
      if(grepl("DV",func)) panel.residual(x,y,..., cex = dotSize)
      if(grepl("CWRES",func)) panel.cwres(x,y, ..., cex = dotSize)
      if(grepl("run.info",func)) {
        panlim = current.panel.limits()
        ltext(0.5,0.5, paste("OFV",
            unique(as.character(round(as.numeric(DATA$variable[subscripts]),1))))
        , cex = textSize)
      }
    }
    , ylab = list("Residual or Predicted", cex = 1)
    , xlab = list("x Variable", cex = 1)
    , par.settings = latticeExtra::ggplot2like()
    , xscale.components = myXscale
    #, yscale.components = myYscale
    )
  )
}


