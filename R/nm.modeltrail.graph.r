# name:     nm.modeltrail.graph.r
# purpose:  create graphical presentation of Bayesian model building trail
# input:    runs (character vector), path (character), and optional color items
# output:   plot with 1 column and length(runs) rows
# note:     depends on qP function read.ext.r     

# ROXYGEN Documentation
#' Graphical Bayesian model trail
#' @description Create graphical presentation of Bayesian model building trail. The function reads the iterations from the run.ext file and displays them as densities in a lattice graph
#' @param runs charater vector with run rootname (e.g. \code{c("run1","run2","run3")})
#' @param path directory where \code{runs} reside
#' @param file.ext file extension of the run.ext file. Defaults to ".ext"
#' @param col.area polygon color of the density plot
#' @param col.mean color of the mean line
#' @param col.percentiles color of the percentile lines
#' @param relabel user-specified labels appearing in the Lattice strips
#' @param strip.size scalar to for the strip size in case strip text is too small ortoo large
#' @param ... graphical arguments passed on to the lattice plots

#' @seealso \code{\link{read.ext}}
#' @return Lattice graph with the distributions of the MCMC Bayesian OFV, supplied with mean line and 2.5th and 97.5th percentiles 
#' @export
#' @import lattice
#' @importFrom stats quantile
#' @examples 
#' ## when there is no Bayesian estimation
#' nm.modeltrail.graph(runs = c("example1","example2")
#'                     , relabel = c("structural model", "final covariate model")
#'                     , file.ext = ".mext"
#'                     , path = getOption("qpExampleDir")
#' )
nm.modeltrail.graph =
function(runs,                                                          ## runs for which file ".ext" has been created
  path = getOption("nmDir"),                                                         ## this is where the NONMEM runs reside
  file.ext = ".ext",
  col.area = gray[5],
  col.mean = red[7],
  col.percentiles = gray[5],                                            ## user can select pdf or wmf
  relabel = NULL,                                                       ## user-specified labels appearing in the Lattice strips
  strip.size = NULL,
  ...
  )
{
  if(!is.null(relabel) & length(relabel) != length(runs)) 
  {
    message("length of relabel argument must be the same as length of runs argument")
    return()
  }
  
  #runs = c('run116','run118'); x = runs[[1]]
  name.runs = runs ;# runs = name.runs
  runs = lapply(runs, function(run, path, file.ext) 
     read.ext(run, path = path, file.ext = file.ext)
     , path = path, file.ext = file.ext
     )

  ## check if we're dealing with Bayesian runs?
  name.objv = lapply(runs, function(x) unlist(lapply(x, function(y) names(y)[length(names(y))])))
  names(name.objv) = name.runs
  test.objv = lapply(name.objv, function(x) lapply(x, function(y) any(y == "MCMCOBJ")))
  
  checked = rep(TRUE, length(runs))
  
  if(any(unlist(test.objv) == FALSE) )
     {
    cat(paste(names(unlist(test.objv)[!unlist(test.objv)]), collapse = "\n"),
        "\n ... are not Bayesian runs and will be discarded.\n")
    if(all(unlist(test.objv) == FALSE)) {
      cat("As none of the runs supplied are Bayesian runs the entire routine aborted\n")
      return()
    }
    checked = unlist(lapply(test.objv, function(x) any(unlist(x))))
    runs = runs[checked]
    names(runs) = names(checked[checked])
  }

  ## OK now we've filtered out the runs that do not contain ANY MCMCOBJ
  ## filter out any other estimation jobs
  runs = lapply(runs, function(x) x[names(x) == "MCMC Bayesian Analysis"])
  runs = lapply(runs, function(x) x[[1]]) 
  
  ## note keep order of relabel and runs in sync
  if(is.null(relabel)) relabel = name.runs[checked] else relabel = relabel[checked]
  
  ## proceed to plotting
  dfr = data.frame(objv = unlist(lapply(runs, function(x) x$MCMCOBJ[x$ITERATION>0])))
  dfr$model = rep(relabel, unlist(lapply(runs, function(x) length(x$MCMCOBJ[x$ITERATION>0]))))
  dfr$model = ordered(dfr$model, levels = relabel)

  if(is.null(strip.size)) strip.size = 3/(1+sqrt(lunique(dfr$model)))
  
  densPlot = densityplot(~ objv | model, 
      data = dfr,
      layout = c(1,lunique(dfr$model)),
      panel = function(x,...)
      {
       panel.densitystrip(x, y=0, col = col.area, horizontal = TRUE, factor = 1, col.line = gray[5])
       panel.abline(v = mean(x), col = col.mean, lwd = 3)
       panel.abline(v = quantile(x,c(0.025,0.975)), lty = 2, col = col.percentiles)
      },
      ylim = c(0,1.02),
      par.strip.text = list(cex = strip.size),
      xlab = "MCMC Bayesian Objective Function Value",
      ...
    )
  
  densPlot
}
