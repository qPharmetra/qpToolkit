# name:     trellis.strip.color
# purpose:  set the trellis strip color scheme to different shades of grey
# input:    none
# output:   lattice panel settings (invisible)
# note:

# ROXYGEN Documentation
#' Set strip colors to gray scales
#' @param strip.colors characte vector of 3 gray scales
#' @return A new trellis device is started with gray strip colors
#' @export
#' @import lattice
#' @importFrom grDevices rgb

trellis.strip.color <- function(
  strip.colors = c(	rgb(200/230,200/230,230/255), #    		lightgrey
                    rgb(243/255,243/255,243/255), #      very	lightgrey
                    rgb(249/255,249/255,249/255)) #     ultra	lightgrey
)
{
  s.b <- trellis.par.get("strip.background")
  s.b$col <- strip.colors
  trellis.par.set("strip.background", s.b)
  s.s <- trellis.par.get("strip.shingle")
  s.s$col <- 0
  trellis.par.set("strip.shingle", s.s)
  invisible()
}

if(F)
{
  xyplot(value~ time | format(dose) * type, data = pkpdData)
  trellis.strip.color()
  xyplot(value~ time | format(dose) * type, data = pkpdData)
}
