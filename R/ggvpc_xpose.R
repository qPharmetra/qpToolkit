
# ROXYGEN Documentation
#' VPC plot using ggplot2 (Xpose-alike version)
#' @description Plot basic vpc with 95\% CI of 2.5th, 50th & 97.5th predicted percentile and observed data.  This call is to be extended with for stratification, and axes definition using the layering custom to ggplot2 objects. See the examples below to learn more.
#' @param vpc output from \code{nm.read.vpc} 
#' @param PI prediction interval (c(0.025,0.975) for 95\% CI)
#' @param area.col.central color of prediction polygon for the central tendency
#' @param area.col.outer color of prediction polygon for the outer percentiles
#' @param linecol.pred color of predicted lines
#' @param linesize.pred line width of of predicted lines
#' @param linetype.obs line type of predicted lines
#' @param linecol.obs line color of observed data
#' @param linesize.obs line width of observed data
#' @param alpha transparancy scalar (between 0 and 1)
#' @param point.shape numeric value for dot shape
#' @param point.size scalar of the observed data dot size
#' @param point.col color of observed data dots
#' @param yrange.stretch vector of c(min,max) which will proportionally rescale the lower and upper limits of the Y axis
#' @return A ggplot object to be extended optionally
#' @export
#' @note Editing and stratification to be done by adding ggplot layer
#' @seealso \code{\link{nm.read.vpc}},  \code{\link{ggvpc_standard}} 
#' @import ggplot2 
#' @examples
#' library(ggplot2)
#' ## example of vpc NOT involving stratification
#'  nm.read.vpc(path =  file.path(getOption("qpExampleDir"),"vpc_final_strt")) -> vpc.all
#' 
#' ggvpc_xpose(vpc.all) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)")
#' 
#' ggvpc_xpose(vpc.all) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)") +
#'   scale_y_log10() + scale_x_log10()
#' 
#' ## demonstration of changing colors & stratification
#' ggvpc_xpose(vpc.all
#'             , area.col.outer = qp.green
#'             , area.col.central = qp.blue
#'             , linecol.pred = steel
#'             , alpha = 1
#'             , linesize.pred=3
#'             , PI = c(0.05,0.95)) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)") +
#'   scale_y_log10() + scale_x_log10() + facet_grid(~strata)
#' 
#' ## PI smoothed
#' nm.read.vpc(path =  file.path(getOption("qpExampleDir"),"vpc_final_strt"),PI.ci.area.smooth=TRUE) -> vpc.all
#' ggvpc_xpose(vpc.all
#'             , PI = c(0.05,0.95)) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)") +
#'   scale_y_log10() + scale_x_log10() + facet_grid(~strata)
#' 

ggvpc_xpose = function(vpc
                       , PI = c(0.025,0.975)
                       , area.col.central = PI.ci.med.arcol
                       , area.col.outer = gray(0.2)
                       , linecol.pred = "blue"
                       , linesize.pred = 1
                       , linetype.obs = "dashed"
                       , linecol.obs = "black"
                       , linesize.obs = 0.5
                       , alpha = 0.33
                       , point.shape = 1
                       , point.size = 1.25
                       , point.col = "darkslategrey"
                       , yrange.stretch = c(0.9,1.1)
                       , quiet = TRUE
)
{
  if(!quiet) message("preparing plot ...")
  
  #require(ggplot2)
  names(vpc$obs)[names(vpc$obs)==unique(vpc$obs$idv.var)]="IVAR"
  names(vpc$obs)[names(vpc$obs)==unique(vpc$obs$dv.var)]="DVVAR"
  
  PI = PI*100
  
  cin = names(vpc$vpc)[grepl("CI.for", names(vpc$vpc))][1]
  cin = sub("vpc","", unPaste(cin, sep="[.]")[[1]])
  
  vpc$res$piLowerDown   = vpc$res[, paste0("vpc",cin,".CI.for.",PI[1],".from")]
  vpc$res$piLowerUp     = vpc$res[, paste0("vpc",cin,".CI.for.",PI[1],".to")]
  vpc$res$piUpperDown   = vpc$res[, paste0("vpc",cin,".CI.for.",PI[2],".from")]
  vpc$res$piUpperUp     = vpc$res[, paste0("vpc",cin,".CI.for.",PI[2],".to")]
  vpc$res$piCentralDown = vpc$res[, paste0("vpc",cin,".CI.for.50.from")]
  vpc$res$piCentralUp   = vpc$res[, paste0("vpc",cin,".CI.for.50.to")]
  
  vpc$vpc$obsLower = vpc$vpc[, paste0("vpc",PI[1],".real")]
  vpc$vpc$obsUpper = vpc$vpc[, paste0("vpc",PI[2],".real")]
  
  p = ggplot() + theme_bw() +
    geom_ribbon(data = vpc$res, aes(x=xCov,ymin=piLowerDown,ymax=piLowerUp)
                , fill=area.col.outer,alpha=alpha) +
    geom_ribbon(data = vpc$res, aes(x=xCov,ymin=piUpperDown,ymax=piUpperUp)
                , fill=area.col.outer,alpha=alpha) +
    geom_ribbon(data = vpc$res, aes(x=xCov,ymin=piCentralDown,ymax=piCentralUp)
                , fill=area.col.central,alpha=alpha) +
    
    geom_line(data = vpc$vpc, aes(x=xCovm,y=obsLower),linetype=linetype.obs, size=linesize.obs) +
    geom_line(data = vpc$vpc, aes(x=xCovm,y=vpc50.real),linetype=linetype.obs, size=linesize.obs) +
    geom_line(data = vpc$vpc, aes(x=xCovm,y=obsUpper),linetype=linetype.obs, size=linesize.obs) +
    coord_cartesian(ylim = yrange.stretch * range(vpc$obs$DV)) +
    geom_line(data = vpc$vpc, aes(x=xCovm, y=vpc50.sim), color=linecol.pred, alpha=0.75, size=linesize.pred) +
    geom_point(data=vpc$obs, aes(x=IVAR,y=DVVAR), col = point.col, shape = point.shape, size = point.size) 
   
  p
}

