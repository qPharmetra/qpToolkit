
# ROXYGEN Documentation
#' qPharmetra Colors
#' @description a set of qPharmetra colors to be used for plotting
#' @section Notes:
#' Colors are named rgb codes
#' @return qPharmetra logo colors qp.blue, qp.green, and the following low saturated colors: lime,apple,emerald, cobalt, cyan, cobalt, indigo, violet, pink, magenta, crimson, ketchup, orange,amber,yellow,brown,olive,steel,mauve,taupe.
#' @export qp.colors qp.green qp.blue qp.colors qp.colors.sorted lime apple emerald cobalt cyan cobalt indigo violet pink magenta crimson ketchup orange amber yellow brown olive steel mauve taupe gray blue green red purple teal obs.color loi.color smooth.color smooth.ci.color lin.fit.col abline.color box.plot.color histogram.color abline.color.box ipred.color pred.color PI.ci.med.arcol PI.real.med.col PI.real.down.col PI.real.up.col PI.ci.down.arcol PI.ci.up.arcol pred.lty ipred.lty
#' @examples
#' ## the greens and the blues
#' par(mfrow = c(1,1), pty = 's')
#' nc = length(blue)
#' cols = c(8,8,8,8,8,4,2,1,1,1)

#' plot(1:nc, seq(-0.45,0.3,length=nc), type = 'n', axes = FALSE, xlab ="", ylab = "",
#'  xlim = c(0.5, nc + 0.5))
#' for(i in 1:nc)
#' {
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(0.16, 0.16, 0.29, 0.29),
#'           col = green[i], angle = -1, density = -1, border=0)
#'   text(i,0.225,paste("green[",i,"]", sep = ""), cex = 0.6, col = green[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(0.01, 0.01, 0.14, 0.14),
#'           col = blue[i], angle = -1, density = -1, border=0)
#'   text(i,0.075,paste("blue[",i,"]", sep = ""), cex = 0.6, col = blue[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(-0.01, -0.01, -0.14, -0.14),
#'           col = red[i], angle = -1, density = -1, border=0)
#'   text(i,-0.075,paste("red[",i,"]", sep = ""), cex = 0.6, col = red[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(-0.16, -0.16, -0.29, -0.29),
#'           col = purple[i], angle = -1, density = -1, border=0)
#'   text(i,-0.225,paste("purple[",i,"]", sep = ""), cex = 0.6, col = purple[cols][i])
#'   polygon(c(i-0.45, i+0.45, i+0.45, i-0.45), c(-0.31, -0.31, -0.44, -0.44),
#'           col = gray[i], angle = -1, density = -1, border=0)
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
#' polygon(x = c(0,1,1,0),y=c(0.5,0.5,0,0), col = qp.blue, border = FALSE)
#' polygon(x = c(0,1,1,0),y=c(1,1,0.5,0.5), col = qp.green, border = FALSE)
#' text(0.5,0.75, "qPharmetra", col = "white", cex = 5)
#' text(0.5,0.25, "qPharmetra", col = "white", cex = 5)
#' 

qp.blue = rgb(20/255,74/255,144/255)
qp.green = rgb(76/255,181/255,79/255)

nc = 7
blue  	= rgb(seq(230,0,length=nc),seq(230,0,length=nc),seq(255,240,length=nc),maxColorValue = 255) 
green		= rgb(seq(230,0,length=nc),seq(255,150,length=nc), seq(230,0,length=nc), maxColorValue = 255)
red		= rgb(seq(255,240,length=nc),seq(230,10,length=nc), seq(230,10,length=nc), maxColorValue = 255)
purple	= rgb(seq(255,240,length=nc),seq(230,10,length=nc), seq(255,240,length=nc), maxColorValue = 255)
gray        = rgb(seq(242,0,length=10),seq(242,0,length=10),seq(242,0,length=10), maxColorValue = 255)

## add superdark example as eighth option
blue  = c(blue , rgb(0,0,150,maxColorValue = 255),rgb(0,0,80,maxColorValue = 255), rgb(0,0,30,maxColorValue = 255))
green = c(green, rgb(0,150,0,maxColorValue = 255),rgb(0,80,0,maxColorValue = 255),rgb(0,30,0,maxColorValue = 255))
red   = c(red  , rgb(150,0,0,maxColorValue = 255),rgb(80,0,0,maxColorValue = 255),rgb(30,0,0,maxColorValue = 255))
purple= c(purple, rgb(150,0,150,maxColorValue = 255),rgb(80,0,80,maxColorValue = 255),rgb(30,0,30,maxColorValue = 255))

nc = length(blue)
names(blue) = paste("blue", 1:nc,sep = "")
names(green) = paste("green", 1:nc,sep = "")
names(red) = paste("red", 1:nc,sep = "")
names(purple) = paste("purple", 1:nc,sep = "")
names(gray) = paste("gray", 1:nc,sep = "")


lime =  rgb(164/255,196/255,0/255)
apple = rgb(96/255,169/255,23/255)
emerald = rgb(0,138/255,0)
teal = rgb(0,171/255,169/255)
cyan = rgb(27/255,161/255,226/255)
cobalt = rgb(0,80/255,239/255)
indigo = rgb(106/255,0,1)
violet = rgb(170/255,0,1)
pink = rgb(244/255,114/255,208/255)
magenta = rgb(216/255,0,115/255)
crimson = rgb(162/255,0,37/255)
ketchup = rgb(229/255,20/255,0)
orange = rgb(250/255,104/255,0)
amber = rgb(240/255,163/255,10/255)
yellow = rgb(227/255,200/255,0)
brown = rgb(130/255,90/255,44/255)
olive = rgb(109/255,135/255,100/255)
steel = rgb(100/255,118/255,135/255)
mauve = rgb(118/255,96/255,138/255)
taupe = rgb(135/255,121/255,78/255)


qp.colors = c(qp.blue = qp.blue,
              qp.green = qp.green, 
              ketchup = ketchup, 
              amber = amber, 
              brown = brown, 
              cobalt = cobalt,
              emerald = emerald, 
              orange = orange, 
              indigo = indigo, 
              apple = apple, 
              cyan = cyan, 
              violet = violet, 
              pink = pink, 
              steel = steel, 
              crimson = crimson, 
              yellow = yellow, 
              teal = teal,
              olive = olive, 
              lime = lime, 
              magenta = magenta, 
              mauve = mauve, 
              taupe = taupe
)

qp.colors.sorted =  c(
   lime = lime, 
   apple = apple, 
   qp.green = qp.green,
   emerald = emerald,
   teal = teal,
   cyan = cyan, 
   cobalt = cobalt,
   qp.blue = qp.blue,
   indigo = indigo, 
   violet = violet,
   pink = pink,
   magenta = magenta,
   ketchup = ketchup, 
   crimson = crimson,
   brown = brown,
   orange = orange,
   amber = amber, 
   yellow = yellow, 
   olive = olive, 
   taupe = taupe, 
   mauve = mauve, 
   steel = steel
)

## autographs colors

obs.color="slategrey"
loi.color="black" 
smooth.color=emerald
smooth.ci.color=emerald 
lin.fit.col="blue"
abline.color="darkslategrey"
box.plot.color="#2C858D"
histogram.color="#2C858D"
abline.color.box="#6A2D8A"
pred.color=ketchup
ipred.lty = 1
ipred.color=cobalt 
pred.lty = 1
PI.ci.med.arcol = emerald
PI.real.med.col= "blue"
PI.real.down.col="darkslategrey"
PI.real.up.col="darkslategrey"
PI.ci.down.arcol = gray(0.2)
PI.ci.up.arcol = gray(0.2)

