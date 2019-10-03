
# ROXYGEN Documentation
#' Lattice panel function for spagghetti plots with a mean line
#' @param x,y The regular x, y input for panel functions
#' @param add.legend logical (T) determining if a legend should be drawn
#' @param legend.panel.row row number of the legend in the x by x grid of panels
#' @param legend.panel.column column number of the legend in the x by x grid of panels
#' @param legend.x x coordinate within the panel
#' @param legend.y y coordinate within the panel
#' @param subscripts default lattice pass-through
#' @param groups default lattice pass-through
#' @param sumText text to describe the summary function ("mean")
#' @param average.lwd line width of average line
#' @param average.col color of average line
#' @param myFun the summarization function (mean)
#' @param type like the lattice argument type for line type
#' @param individual.col = color of individual lines (spagghetti elements)
#' @param show.points logical indicating to show observed data dots 
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @examples 
#' \dontrun{
#' library(lattice)
#' library(metrumrg)
#' pkpdData = example.pkpdData()
#' xyplot(value ~ time | dose *  type
#'        , data = pkpdData
#'        , subset = dose > 0 & value > 0
#'        , groups = id
#'        , aspect = 1
#'        , panel = panel.meanspag
#' )
#' 
#' out = get.xpose.tables(file.path(getOption("qpExampleDir"),"example2"))
#' trellis.strip.color()
#' xyplot(CONC ~ TIME 
#'        , data = out
#'        , groups = ID
#'        , subset = EVID==0
#'        , aspect = 1
#'        , scales = list(x = list(relation = "free"), y = list(log = 10))
#'        , panel = panel.meanspag
#'        , yscale.components = yscale.components.log10.3
#' )
#'
#'# in case there are too many times to average by then the following might be #'#better interpretable
#'ptimes = seq(min(out$TIME),max(out$TIME), length = 14)
#'xyplot(CONC ~ Cbind(TIME, metrumrg::snap(TIME, rule = ptimes))  
#'       , data = out
#'       , groups = ID
#'       , subset = EVID==0
#'       , aspect = 1
#'       , scales = list(x = list(relation = "free"), y = list(log = 10))
#'       , panel = function(x,y,...)
#'       {
#'         panel.xyplot(x,y,..., col = gray[5], type = 'l')
#'         yy = tapply(y, attr(x, "other")[,1], mean)
#'         llines(as.numeric(names(yy)), yy, col = red[8], lwd = 2)
#'      }
#'       , yscale.components = yscale.components.log10.3
#')
#'}
panel.meanspag = 
  function(x, y, subscripts, groups, ..., 
           myFun = mean, 
           sumText = "Mean", 
           individual.col = gray[5], type = "l", 
           average.lwd = 3, average.col = red[8],
           add.legend = TRUE, legend.panel.row, legend.panel.column, legend.x, legend.y,
           show.points=FALSE
  )
  {
    panel.superpose(x,y, subscripts, groups, ..., col = individual.col, type = type)
    
    panel.avg.LP(x,y, fun = myFun, col = average.col, lwd = average.lwd, horizontal = FALSE
                 , type="b", show.points=show.points, pch=16)
    if(add.legend) {
      if(missing(legend.panel.row)) legend.panel.row = 1
      if(missing(legend.panel.column)) legend.panel.column = 1
      #browser()
      if(current.row() == legend.panel.row & current.column() == legend.panel.column) {
        panel.width = current.panel.limits()$xlim[2]-current.panel.limits()$xlim[1]
        panel.height = current.panel.limits()$ylim[2]-current.panel.limits()$ylim[1]
        if(missing(legend.x)) {
          legend.x = current.panel.limits()$xlim[1] + 0.7*panel.width
        }
        if(missing(legend.y)) {
          legend.y = current.panel.limits()$ylim[1] + 0.95*panel.height
        }
        panel.lines(c(legend.x,legend.x+0.1*panel.width), rep(legend.y,2),
                    col = average.col, lwd = average.lwd)
        panel.text(legend.x+0.12*panel.width, legend.y+.003*panel.height, sumText, cex=0.8, adj = 0)
        panel.lines(c(legend.x,legend.x+0.1*panel.width),
                    rep(legend.y-0.05*panel.height,2),
                    col = individual.col, type = type)
        panel.text(legend.x+0.12*panel.width, legend.y-0.047*panel.height, "Observed", cex=0.8, adj = 0)
      }
    }
  }


