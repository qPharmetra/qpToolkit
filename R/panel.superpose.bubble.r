
# ROXYGEN Documentation
#' Panel function for bubble plots
#' @description  Panel function to create bubble plots where the symbol size varyies in proportion to a variable
#' @param x,y numeric vectors x and y
#' @param n the column containing the sample ie of each record
#' @param cols bubble colors
#' @param syms bubbles symbol types
#' @param groups grouping levels
#' @param subscripts subscripts
#' @param \dots passed to llines() and lpoints()
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @examples
#' library(lattice)
#' myData = data.frame(  n = c(5,100,332,12,50,74)
#' , value = c(0.13,0.78,0.49,0.67,0.34,0.57)
#' , dose = c(20,25,50,30,40,35)
#' , sex = rep(c("M","F"), ea = 3)
#' )
#' myData = expand.data(myData, values = c(0,1,2,3,4), name= "time")
#' myData$ypred = myData$value * myData$time ^3/(1 + myData$time^3)
#' xyplot(ypred ~ time
#'        , data = myData
#'        , groups = dose
#'        , subscripts = TRUE
#'        , nn = sqrt(myData$n/10)
#'        , panel = function(x,y, nn = nn, groups, subscripts,...)
#'        {
#'          panel.superpose.bubble(x,y,nn, groups, subscripts, ...)
#'        }
#' )

panel.superpose.bubble <- function(x, y, n, groups, subscripts, cols=NULL, syms=NULL,...)
{
  if(is.null(cols)) {
    ds = data.frame(x=x, y=y, n=n[subscripts], groups=groups[subscripts])
    ugrps = unique(ds$groups)
  } else {
    if(is.null(syms)) syms = rep(1, length(x)) else syms=syms[subscripts]
    ds = data.frame(x=x, y=y,  n=n[subscripts], groups=groups[subscripts], cols=cols[subscripts]
                    , syms=syms)
    ds = ds[order(ds$cols),]  # Put strongest colors on top
    ugrps = unique(ds$groups)
  }

  for(i in seq_along(ugrps))  ## for each group level do the following
  {
    xx = ds$x[ds$groups == ugrps[i]]
    yy = ds$y[ds$groups == ugrps[i]]
    nn = ds$n[ds$groups == ugrps[i]]
    if(is.null(cols)) {
      mycol=i
      sym = 1
    } else{
      mycol = unique(ds$cols[ds$groups==ugrps[i]])
      sym = unique(ds$syms[ds$groups==ugrps[i]])
    }
    llines(xx[order(xx)], yy[order(xx)], col = mycol, type = "l", ...)
    for(dot in 1:length(xx))  ## for each data point in the group do:
      lpoints(xx[dot], yy[dot], cex = ifelse(is.na(nn[dot]),1,nn[dot]), col = mycol, pch=sym, ...)
  } ## close groups loop
}

