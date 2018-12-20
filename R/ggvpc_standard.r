# name:     ggvpc_standard
# purpose:  
# input:    output from nm.read.vpc
# output:   a ggplot object
# note:     editing and stratification to be done by adding ggplot layer

# ROXYGEN Documentation
#' VPC plot using ggplot2 (standard)
#' @description Plot basic vpc with 95\% PI and observed data. This call is to be extended with for stratification, and axes definition using the layering custom to ggplot2 objects. See the examples below to learn more.
#' @param vpc output from \code{nm.read.vpc} 
#' @param PI prediction interval (c(0.025,0.975) for 95\% CI)
#' @param area.col color of prediction polygon
#' @param linecol.pred color of predicted lines
#' @param linesize.pred line width of of predicted lines
#' @param linetype.obs line type of predicted lines
#' @param linecol.obs line color of observed data
#' @param linesize.obs line width of observed data
#' @param alpha transparancy scalar (between 0 and 1)
#' @param point.shape numeric value for dot shape 
#' @param point.col color of observed data dots
#' @param yrange.stretch vector of c(min,max) which will proportionally rescale the lower and upper limits of the Y axis
#' @return A ggplot object to be extended optionally
#' @export
#' @note Editing and stratification to be done by adding ggplot layer
#' @seealso \code{\link{nm.read.vpc}},  \code{\link{ggvpc_xpose}} 
#' @import ggplot2 
#' @examples
#' vpc.all = nm.read.vpc(path = file.path(getOption("qpExampleDir"), "vpc_final_strt"))
#' ggvpc_standard(vpc.all) 
#' 
#' ggvpc_standard(vpc.all) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)")
#' 
#' ## different Prediction Interval (PI), let's use 90% instead of 95%
#' ggvpc_standard(vpc.all, PI = c(0.05,0.95)) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)")
#' 
#' ## logging axes
#' ggvpc_standard(vpc.all) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)") + scale_y_log10() + scale_x_log10()
#' 
#' #modify colors and transparency:
#' ggvpc_standard(vpc.all, area.col = indigo, linecol.pred = ketchup, alpha = 1, point.shape = 15) + 
#'   labs(x="Time (h)", y="Concentration (ng/ml)") + scale_y_log10()  
#' 
#' ## dealing with stratification
#' myVPC = nm.read.vpc(path = file.path(getOption("qpExampleDir"), "vpc_base_strt"))
#'
#' p = ggvpc_standard(vpc.all, yrange.stretch = c(1,1)) 
#' p = p +  labs(x="Time (h)", y="Concentration (ng/ml)")  
#' p +  facet_wrap(~strata) + scale_y_log10()



ggvpc_standard = function(vpc
                          , PI = c(0.025,0.975)
                          , area.col = PI.ci.med.arcol
                          , linecol.pred = PI.real.med.col
                          , linesize.pred = 1
                          , linetype.obs = "dashed"
                          , linecol.obs = "black"
                          , linesize.obs = 0.5
                          , alpha = 0.33
                          , point.shape = 1
                          , point.size = 1.5
                          , point.col = "darkslategrey"
                          , yrange.stretch = c(0.9,1.1)
                          , quiet = T
)
{
  #PI = c(0.025,0.975); area.col = cobalt;linecol.pred = ketchup;linesize.pred = 1; linetype.obs = "dashed";linecol.obs = "black";linesize.obs = 0.5;alpha = 0.5;point.shape = 15;point.col = "black";yrange.stretch = c(0.9,1.1)
   if(!quiet) message("preparing plot ...")
  
  #require(ggplot2)
  names(vpc$obs)[names(vpc$obs)==unique(vpc$obs$idv.var)]="IVAR"
  names(vpc$obs)[names(vpc$obs)==unique(vpc$obs$dv.var)]="DVVAR"
  
  PI = PI*100
 
  vpc$res$piLower = vpc$res[, paste0("vpc",PI[1],".sim")]
  vpc$res$piUpper = vpc$res[, paste0("vpc",PI[2],".sim")]
  vpc$vpc$obsLower = vpc$vpc[, paste0("vpc",PI[1],".real")]
  vpc$vpc$obsUpper = vpc$vpc[, paste0("vpc",PI[2],".real")]
  
  
  p = ggplot() + theme_bw() +
    geom_point(data=vpc$obs, aes(x=IVAR,y=DVVAR), alpha = alpha, shape = point.shape, col = point.col, size=point.size) +
    geom_ribbon(data = vpc$res, aes(x=xCov,ymin=piLower,ymax=piUpper), fill=area.col,alpha=alpha) +    
    geom_line(data = vpc$vpc, aes(x=xCovm,y=obsLower),linetype=linetype.obs, size=linesize.obs) +
    geom_line(data = vpc$vpc, aes(x=xCovm,y=vpc50.real),linetype=linetype.obs, size=linesize.obs) +
    geom_line(data = vpc$vpc, aes(x=xCovm,y=obsUpper),linetype=linetype.obs, size=linesize.obs) +
    geom_line(data = vpc$vpc, aes(x=xCovm, y=vpc50.sim), color=linecol.pred, alpha = 0.75,size=linesize.pred) +
    coord_cartesian(ylim = yrange.stretch * range(vpc$obs$DV)) +
    geom_point(data=vpc$obs, aes(x=IVAR,y=DVVAR), shape = point.shape, col = point.col, size=point.size)
  p
}

