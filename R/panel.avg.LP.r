# ROXYGEN Documentation
#' Lattice panel function for average lines
#' @description Panel function that can be used with xyplot() (lattice) and related functions to plot the average (or other metric) of y values at each X, while also drawing connecting lines and points. This is modified version of panel.average, and is used by \code{panel.meanspag()}
#' @param x,y The regular x, y input for panel functions
#' @param fun function to summarize by, defaults to \code{mean}
#' @param lty line type (taken from \code{trellis.par.get("reference.line")})
#' @param horizontal logical (T) indicating if the average needs to be taken vertical or horizontal
#' @param lwd line width (taken from \code{trellis.par.get("reference.line")})
#' @param col.line line color (taken from \code{trellis.par.get("reference.line")})
#' @param col observed data color
#' @param type same as type in a lattice call (default is "l" for lines)
#' @param identifier option to join lines (which is default)
#' @param show.points logical indicating to show observed data dots 
#' @return Nothing -> internal to a lattice call
#' @export
#' @import lattice

panel.avg.LP = function (x, y, fun = mean
                         , horizontal = TRUE, lwd = reference.line$lwd, 
                         lty = reference.line$lty, col, col.line = 
                         reference.line$col, type = "l", ...,
                         identifier = "linejoin", show.points=FALSE) 
{# modified version of panel.average so that lines and points can be produced, used
  # in panel.meanspag
  x <- as.numeric(x)
  y <- as.numeric(y)
  reference.line = trellis.par.get("reference.line")
  if (!missing(col)) {
    if (missing(col.line)) 
      col.line <- col
  }
  if (horizontal) {
    vals <- unique(sort(y))
    yy <- seq_along(vals)
    xx <- numeric(length(yy))
    for (i in yy) xx[i] <- fun(x[y == vals[i]])
    if(show.points) panel.points(xx, vals[yy], col = col.line, cex=1.1,..., identifier=identifier)
    panel.lines(xx, vals[yy], col = col.line, lty = lty, 
                lwd = lwd, ..., identifier = identifier)
  }
  else {
    vals <- unique(sort(x))
    xx <- seq_along(vals)
    yy <- numeric(length(xx))
    for (i in xx) yy[i] <- fun(y[x == vals[i]])
    if(show.points) panel.points(vals[xx], yy, col = col.line, cex=1.1,..., identifier=identifier)
    panel.lines(vals[xx], yy, col = col.line, lty = lty, 
                lwd = lwd, ..., identifier = identifier)
  }
}


# Examples
if (F) {
  set.seed(123456)
  pData = data.frame( id=rep(1:50,each=5),
                      x=rep(1:50,5),
                      y=rnorm(50*5,30,3))
  
  
  xyplot(y ~ x, data=pData,
         panel=function(x,y,...) {
           panel.avg.LP(x,y, fun=mean, col=red[8], lwd=3, horizontal=FALSE, type="b", show.points=TRUE, pch=16)
         }
  )
  
  # note the panel function "meanspag" itself uses panel.avg.LP()
  xyplot(y ~ x, data=pData
                , groups = id
                , type="l"
                , panel=function(x,y,...){
                   panel.meanspag(x,y,..., add.legend=FALSE, show.points=TRUE )
                }
                , ylab = list("X stuff", cex=1)
                , xlab = list("Y stuff", cex=1)
  )
}