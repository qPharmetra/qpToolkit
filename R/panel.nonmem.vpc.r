# name:     panel.nonmem.vpc
# purpose:  panel function to plot PsN's vpc output
# input:    many numeric inouts
# output:   n.a.
# note:     is used inside nm.vpcplot exclusively

# ROXYGEN Documentation
#' Lattice function for predicted components of a NONMEM VPC
#' @description  Panel function for predicted and observed lines / colored polygons of a vpc plot of an NONMEM / PsN VPC object
# @param other a collection of parameters are available to modify the layout of the VPC
#' @param x x variable
#' @param y y variable
#' @param OBS list with element: strata_no
#' @param vpc list with element: strata_no
#' @param subscripts subscripts for vpc
#' @param logY logical for logging Y coordinate
#' @param logX logical for logging X coordinate
#' @param showPredAs choice of 'lines', 'area', or 'xpose'
#' @param showObsDots whether to show observations as dots
#' @param showObsLines whether to show observations as lines
#' @param xCov covariates of interest from vpc
#' @param col.scheme list with elements: [pred: [outer, central, area], obs: [dot, line]]
#' @param obscex.dot cex for observations
#' @param obspch.dot pch for observations
#' @param lineType line type for observations and predictions
#' @param central.lwd line width for predicted central tendency
#' @param \dots passed to panel.xyplot() and llines()
#' @return Lattice panel output (invisible)
#' @note Not to be used as standalone. Will be be called by \code{nm.vpcplot}. Furthermore, the \code{nm.vpcplot} routine will go deprecated soon, thus it is not adviced to use this function going forward. Instead use xpose.VPC.
#' @seealso \code{\link{nm.vpcplot}}
#' @import lattice
#' @importFrom utils head

panel.nonmem.vpc <- function(
  x, y, OBS, vpc, subscripts, logY, logX, showPredAs,
  showObsDots, showObsLines, xCov,
 col.scheme, obscex.dot, obspch.dot, lineType, central.lwd, ...
 ){
  info = data.frame(attr(y, "other"))

  if(showPredAs == "lines")
  {
    yl = info$ypred.lo
    yh = info$ypred.hi
    if(logY) { yh = log10(yh); yl = log10(yl) }
    #if(logX == T) { x = log10(x)}
    llines(x,yl, col = col.scheme$pred$outer,  type = lineType, lwd = 1.5)
    llines(x,yh, col = col.scheme$pred$outer,  type = lineType, lwd = 1.5)
    llines(x,y, ...,  type = lineType, col = col.scheme$pred$central, lwd = 1.5)
  }

  if(showPredAs == "area")
  {
    yl = info$ypred.lo
    yh = info$ypred.hi
    if(logY) {yh = log10(yh); yl = log10(yl)}
    #if(logX == T) { x = log10(x)}
    lpolygon(c(x,rev(x)), c(yl,rev(yh)), col = col.scheme$pred$area, border = FALSE)
  }

  if(showPredAs == "xpose")
  {
    yl = info$ypred.lo
    yld = info$ypred.lo.dn
    ylu = info$ypred.lo.up
    yh = info$ypred.hi
    yhd = info$ypred.hi.dn
    yhu = info$ypred.hi.up
    ycd = info$ypred.cen.dn
    ycu = info$ypred.cen.up
    if(logY) {
      yh = log10(yh); yl = log10(yl);yld = log10(yld)
      ylu = log10(ylu);yhd = log10(yhd); yhu = log10(yhu);ycd = log10(ycd); ycu = log10(ycu)
    }
    #if(logX == T) { x = log10(x)}
    lpolygon(c(x,rev(x)), c(ylu,rev(yld)), col = col.scheme$pred$area,outer, border = FALSE)
    lpolygon(c(x,rev(x)), c(yhu,rev(yhd)), col = col.scheme$pred$area.outer, border = FALSE)
    lpolygon(c(x,rev(x)), c(ycu,rev(ycd)), col = col.scheme$pred$area.central, border = FALSE)
    llines(x, yl,  type = lineType, col = col.scheme$pred$outer, lwd = 1.5)
    llines(x, yh,  type = lineType, col = col.scheme$pred$outer, lwd = 1.5)
  }

  ## central predicted tendency
  llines(x,y, ...,  type = "l", col = col.scheme$pred$central, lwd = central.lwd)

  ## plot observations
  mysubs = OBS$strata_no == unique(vpc$strata_no[subscripts])
  #subscripts = vpc$strata_no == 1
  head(OBS)
  if(logY) OBS[mysubs, 2] = log10(OBS[mysubs, 2])
  if(logX) OBS[mysubs, 3] = log10(OBS[mysubs, 3])
  if(showObsDots) panel.xyplot(OBS[mysubs, 3], OBS[mysubs, 2], ..., cex = obscex.dot,
                               pch = obspch.dot, col = col.scheme$obs$dot, type = 'p')
  if(showObsLines){
    if(logY) {vpc[, c('yobs.lo','yobs.cen', 'yobs.hi')] =
                   log10(vpc[, c('yobs.lo','yobs.cen', 'yobs.hi')])}
    if(logX) {vpc[, xCov] = log10(vpc[, xCov])}

    panel.xyplot(vpc[subscripts,xCov], vpc$yobs.lo[subscripts], ...,
                 col =  col.scheme$obs$line, type = lineType, lty = 2)
    panel.xyplot(vpc[subscripts,xCov], vpc$yobs.cen[subscripts], ...,
                 col =  col.scheme$obs$line, type = lineType)
    panel.xyplot(vpc[subscripts,xCov], vpc$yobs.hi[subscripts], ...,
                 col =  col.scheme$obs$line, type = lineType, lty = 2)
  }
}

# if(F)
# {
#   nm.vpcplot(file.path(nmDir, "vpc2169_all"), logY = TRUE)
#   ## note the plotting is done using panel.nonmem.vpc inside the function
# }

