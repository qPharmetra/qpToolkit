#' qPharmetra Colors
#'
#' A set of qPharmetra colors to be used for plotting
#'
#' @family qpcolors
#' @examples
#' ## the greens and the blues
#' par(mfrow = c(1,1), pty = 's')
#' nc = length(blue)
#' cols = c(8,8,8,8,8,4,2,1,1,1)

#' plot(1:nc, seq(-0.45,0.3,length = nc), type = 'n', axes = FALSE, xlab = "", ylab = "",
#'  xlim = c(0.5, nc + 0.5))
#' for(i in 1:nc)
#' {
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(0.16, 0.16, 0.29, 0.29),
#'           col = green[i], angle = -1, density = -1, border = 0)
#'   text(i,0.225,paste("green[",i,"]", sep = ""), cex = 0.6, col = green[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(0.01, 0.01, 0.14, 0.14),
#'           col = blue[i], angle = -1, density = -1, border = 0)
#'   text(i,0.075,paste("blue[",i,"]", sep = ""), cex = 0.6, col = blue[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(-0.01, -0.01, -0.14, -0.14),
#'           col = red[i], angle = -1, density = -1, border = 0)
#'   text(i,-0.075,paste("red[",i,"]", sep = ""), cex = 0.6, col = red[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(-0.16, -0.16, -0.29, -0.29),
#'           col = purple[i], angle = -1, density = -1, border = 0)
#'   text(i,-0.225,paste("purple[",i,"]", sep = ""), cex = 0.6, col = purple[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(-0.31, -0.31, -0.44, -0.44),
#'           col = gray[i], angle = -1, density = -1, border = 0)
#'   text(i,-0.375,paste("gray[",i,"]", sep = ""), cex = 0.6, col = gray[cols][i])
#' }
#'
#' library(lattice)
#' lapply(list(qp.colors,qp.colors.sorted), function(my.colors)
#' xyplot(yval ~ xval | color
#'       , data = expand.grid(xval = 1, yval = 1, color = names(my.colors))
#'       , xlim = c(0,1)
#'       , ylim = c(0,1)
#'       , as.table = TRUE
#'       , panel = function(x,y,...)
#'       {
#'         panel.rect(xleft = 0, ybottom = 0
#'                    , xright = 1, ytop = 1
#'                    , col = my.colors[panel.number()]
#'         )
#'         ltext(x = 0.5, y = 0.5
#'               , my.colors[panel.number()], col = rgb(0.99,0.99,0.99)
#'               , cex = 0.75
#'         )
#'       }
#'       , aspect = 1
#'       , par.strip.text = list(cex = 1)
#'       , scales = list(y = list(at = NULL), x = list(at = NULL))
#'       , ylab = ""
#'       , xlab = ""
#'    )
#' )
#'
#' par(pty = "m")
#' plot(0:1,0:1, type = 'n',axes = FALSE, xlab = "", ylab = "")
#' polygon(x = c(0,1,1,0),y = c(0.5,0.5,0,0), col = qp.blue, border = FALSE)
#' polygon(x = c(0,1,1,0),y = c(1,1,0.5,0.5), col = qp.green, border = FALSE)
#' text(0.5,0.75, "qPharmetra", col = "white", cex = 5)
#' text(0.5,0.25, "qPharmetra", col = "white", cex = 5)
'qp.colors'

#' qP Colors Sorted
#'
#' A sorted version of qP colors
'qp.colors.sorted'

#' qp.blue
#'
#' @family qpcolors
'qp.blue'
# foo <- Hmisc::Cs(
#    qp.blue,
#    qp.green,
#    blue,
#    green,
#    red,
#    purple,
#    gray,
#    blue,
#    green,
#    red,
#    purple,
#    lime,
#    apple,
#    emerald,
#    teal,
#    cyan,
#    cobalt,
#    indigo,
#    violet,
#    pink,
#    magenta,
#    crimson,
#    ketchup,
#    orange,
#    amber,
#    yellow,
#    brown,
#    olive,
#    steel,
#    mauve,
#    taupe,
#    qp.colors,
#    qp.colors.sorted,
#    # autograph
#    obs.color,
#    loi.color,
#    smooth.color,
#    smooth.ci.color,
#    lin.fit.col,
#    abline.color,
#    box.plot.color,
#    histogram.color,
#    abline.color.box,
#    pred.color,
#    ipred.color,
#    PI.ci.med.arcol,
#    PI.real.med.col,
#    PI.real.down.col,
#    PI.real.up.col,
#    PI.ci.down.arcol,
#    PI.ci.up.arcol
# )
# for(i in foo){
#    writeLines(paste("#'", i))
#    writeLines("#'")
#    writeLines("#' @family qpcolors")
#    writeLines(paste0('"',i,'"'))
#    writeLines('')
# }
#' qp.blue
#'
#' @family qpcolors
"qp.blue"

#' qp.green
#'
#' @family qpcolors
"qp.green"

#' blue
#'
#' @family qpcolors
"blue"

#' green
#'
#' @family qpcolors
"green"

#' red
#'
#' @family qpcolors
"red"

#' purple
#'
#' @family qpcolors
"purple"

#' gray
#'
#' @family qpcolors
"gray"

#' blue
#'
#' @family qpcolors
"blue"

#' green
#'
#' @family qpcolors
"green"

#' red
#'
#' @family qpcolors
"red"

#' purple
#'
#' @family qpcolors
"purple"

#' lime
#'
#' @family qpcolors
"lime"

#' apple
#'
#' @family qpcolors
"apple"

#' emerald
#'
#' @family qpcolors
"emerald"

#' teal
#'
#' @family qpcolors
"teal"

#' cyan
#'
#' @family qpcolors
"cyan"

#' cobalt
#'
#' @family qpcolors
"cobalt"

#' indigo
#'
#' @family qpcolors
"indigo"

#' violet
#'
#' @family qpcolors
"violet"

#' pink
#'
#' @family qpcolors
"pink"

#' magenta
#'
#' @family qpcolors
"magenta"

#' crimson
#'
#' @family qpcolors
"crimson"

#' ketchup
#'
#' @family qpcolors
"ketchup"

#' orange
#'
#' @family qpcolors
"orange"

#' amber
#'
#' @family qpcolors
"amber"

#' yellow
#'
#' @family qpcolors
"yellow"

#' brown
#'
#' @family qpcolors
"brown"

#' olive
#'
#' @family qpcolors
"olive"

#' steel
#'
#' @family qpcolors
"steel"

#' mauve
#'
#' @family qpcolors
"mauve"

#' taupe
#'
#' @family qpcolors
"taupe"

#' obs.color (Autograph)
#'
#' @family qpcolors
"obs.color"

#' loi.color (Autograph)
#'
#' @family qpcolors (Autograph)
"loi.color"

#' smooth.color (Autograph)
#'
#' @family qpcolors
"smooth.color"

#' smooth.ci.color (Autograph)
#'
#' @family qpcolors
"smooth.ci.color"

#' lin.fit.col (Autograph)
#'
#' @family qpcolors
"lin.fit.col"

#' abline.color (Autograph)
#'
#' @family qpcolors
"abline.color"

#' box.plot.color (Autograph)
#'
#' @family qpcolors
"box.plot.color"

#' histogram.color (Autograph)
#'
#' @family qpcolors
"histogram.color"

#' abline.color.box (Autograph)
#'
#' @family qpcolors
"abline.color.box"

#' pred.color (Autograph)
#'
#' @family qpcolors
"pred.color"

#' ipred.color (Autograph)
#'
#' @family qpcolors
"ipred.color"

#' PI.ci.med.arcol (Autograph)
#'
#' @family qpcolors
"PI.ci.med.arcol"

#' PI.real.med.col (Autograph)
#'
#' @family qpcolors
"PI.real.med.col"

#' PI.real.down.col (Autograph)
#'
#' @family qpcolors
"PI.real.down.col"

#' PI.real.up.col (Autograph)
#'
#' @family qpcolors
"PI.real.up.col"

#' PI.ci.down.arcol (Autograph)
#'
#' @family qpcolors
"PI.ci.down.arcol"

#' PI.ci.up.arcol (Autograph)
#'
#' @family qpcolors (Autograph)
"PI.ci.up.arcol"
