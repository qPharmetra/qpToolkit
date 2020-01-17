### _____________________________________________________________________________________________
### CLIENT:		qPharmetra
### PROJECT:	qP PROJECT
###
# name:     plot.tornado
# purpose:  plots 'tornado' horizontal bar chart with distributions ordered by width
# input:    specific data set 'torn.ds' with specifications detailed below
# output:   plot window
# note:

## Further explanation to the tool:
## plot.tornado
## c
##
## Arguments
##	torn.ds		Dataset containing sensitivity analysis results to be plotted.
##					variable	Names of variable (will be plotted as y axis labels)
##					val.lo	low value of objective when variable is swung to an extreme
##					val.base	base value of objective when all variables are at base
##					val.hi	high value of the objective when variable is swung to an extreme
##					lab.lo	label associated with low
##					lab.base	label associated with base
##					lab.hi	label associated with high
##	others		I am hoping these are self-explanatory or can be surmised.
##
## Note:
##	the code ignores all but the FIRST value in the val.base
##
## Tornado algorithm from R.T. Clemen. Making Hard Decision: An Introduction to Decision Analysis, 1st Edn,
##		Duxbury Press, Bellmont, CA, 1991.  Chapter 5, page 116


# ROXYGEN Documentation
#' Tornado plots
#' @description Create a tornado plot given a table of sensitivity analysis results
#' @param x Dataset containing sensitivity analysis results to be plotted. Column names should include: \code{variable} - names of variable (will be plotted as y axis labels), \code{val.lo} - low value of objective when variable is swung to an extreme, \code{val.base}	base value of objective when all variables are at base, \code{val.hi} - high value of the objective when variable is swung to an extreme, \code{lab.lo} -	label associated with low, \code{lab.base} -	label associated with base, \code{lab.hi}	label associated with high
#' @param \dots ignored
#' @param main graphical parameter
#' @param xlim graphical parameter
#' @param xlab graphical parameter
#' @param bar.width graphical parameter
#' @param base.round graphical parameter
#' @param cex.main graphical parameter
#' @param cex.xaxis graphical parameter
#' @param cex.yaxis graphical parameter
#' @param cex.barlab graphical parameter
#' @param cex.baslab graphical parameter
#' @param bar.col graphical parameter
#' @param show.base graphical parameter
#' @param adj.ylabs graphical parameter
#' @return A tornado plot
#' @family tornado
#' @export
#' @importFrom graphics plot
#' @importFrom graphics par
#' @importFrom graphics axis
#' @importFrom graphics polygon
#' @importFrom graphics text
#' @importFrom graphics abline
#' @examples
#' torn.ds = as.tornado(read.csv(
#'   file.path(getOption("qpExampleDir"), "../Excel/tornado.ds.csv")
#' , stringsAsFactors = FALSE))
#' plot(torn.ds
#'              , xlim = c(0,20)
#'              , xlab = "Dollars"
#'              , main = "My first tornado"
#'              , cex.main = 2
#'              , bar.width =.50
#'              , base.round = 1
#'              , show.base = FALSE
#' )

plot.tornado <- function(x, ..., main = "", xlim = range(c(torn.ds$val.hi,torn.ds$val.lo)), xlab = "Output", bar.width =.75, base.round = 0, cex.main = 1.5,
					cex.xaxis = cex.main*0.5, cex.yaxis = cex.main*.65, cex.barlab = cex.main*.55,
					cex.baslab = cex.main*.5, bar.col = "#0A41A5", show.base = TRUE, adj.ylabs = 0)
{
	par(mfrow = c(1,1), mar = c(5,8,8,1))

	torn.ds$range = abs(torn.ds$val.hi-torn.ds$val.lo)
	torn.ds = torn.ds[order(torn.ds$range),]

	plot(1, 1, xlim = xlim, ylim = c(0, 1 + nrow(torn.ds)*2), type = "n", axes = FALSE,
			xlab = xlab, ylab = "", main = main, col = 8, cex = cex.main)
	at = seq(min(xlim), max(xlim), diff(xlim)*0.2)
	axis(side = 1, at = at, labels = as.character(at), cex.axis = cex.xaxis)

	for (i in 1:nrow(torn.ds)) {
		#i  = 1
		xhi = max(torn.ds$val.hi[i], torn.ds$val.lo[i])
		xlo = min(torn.ds$val.hi[i], torn.ds$val.lo[i])
		ydist = (nrow(torn.ds)*2+1)/nrow(torn.ds)
		yhi = ydist + (i-1)*ydist
		ylo = ydist*(1-bar.width) + (i-1)*ydist
		polygon(c(xlo, xhi, xhi, xlo), c(ylo, ylo, yhi, yhi), density =-1, col = bar.col)

		text(par("usr")[1] - adj.ylabs,(yhi+ylo)/2, as.character(torn.ds$variable[i]), adj = 1, cex = cex.yaxis, xpd = TRUE)

		this.lab.lo = ifelse(torn.ds$val.lo[i]<torn.ds$val.hi[i],torn.ds$lab.lo[i],torn.ds$lab.hi[i])
		this.lab.hi = ifelse(torn.ds$val.hi[i]>torn.ds$val.lo[i],torn.ds$lab.hi[i],torn.ds$lab.lo[i])

		text(xlo-diff(xlim)*0.03, (yhi+ylo)/2, as.character(this.lab.lo), adj = 1, cex = cex.barlab)
		text(xhi+diff(xlim)*0.03, (yhi+ylo)/2, as.character(this.lab.hi), adj = 0, cex = cex.barlab)
	}
	abline(v = torn.ds$val.base[1])
	if (show.base) {
		text(torn.ds$val.base[1]*1.03, 0.25, paste("Base = ", as.character(round(torn.ds$val.base[1], base.round)), sep = ""),
			col = 1, adj = 0, cex = cex.baslab)
	}
}

#' Coerce to Tornado
#' @description Coerce something to a tornado object.
#' @param x object to be coerced
#' @param \dots ignored
#' @return object of class \code{tornado}
#' @family tornado
#' @export
as.tornado <- function(x,...)UseMethod('as.tornado')

#' Coerce to Tornado from data.frame
#' @description Coerce data.frame to a tornado object.
#' @param x data.frame
#' @param \dots ignored
#' @return object of class \code{tornado}
#' @family tornado
#' @export
as.tornado.data.frame <- function(x,...){
	expected <- c('variable','val.lo','val.base','val.hi', 'lab.lo','lab.base','lab.hi')
	nms <- names(x)
	missing <- base::setdiff(expected, nms)
	msg <- paste(collapse = ', ', missing)
	if(length(missing)) warning('expecting but not seeing ', msg)
	class(x) <- c('tornado', class(x))
	x
}



