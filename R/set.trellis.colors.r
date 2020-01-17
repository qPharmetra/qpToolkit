
# ROXYGEN Documentation
#' Set Trellis/Lattice colors
#' @description when called before a lattice call, the strip panels will be set in grayscales
#' @param symbol a list with colors for the strips
#' @param line a list with col(or) specified
#' @return Lattice panel output (invisible)
#' @export
#' @import lattice
#' @examples
#' library(lattice)
#' df = example.pkpdData()
#' xyplot(value~ time | format(dose) * type, data = df)
#' set.trellis.colors ()
#' xyplot(value~ time | format(dose), data = df, groups = type)

set.trellis.colors <- function(symbol = list(fill = gray[2],col = gray[6]), line = list(col = gray[7]))
{
  tpg = trellis.par.get()
  ln = tpg$superpose.line
  sb = tpg$superpose.symbol

  whsb = match(names(symbol), names(sb))
  whln = match(names(symbol), names(ln))

  sb[whsb] = symbol
  ln[whln] = line

  trellis.par.set("superpose.line", value = ln)
  trellis.par.set("superpose.symbol", value = sb)
  invisible()
}


