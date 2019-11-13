# name:     meta.map
# purpose:  Draw a mapping of a meta-analysis dataset
# input:    meta-analysis dataset, comparator and reference information
# output:   plot with the mapping relationships in meta-analysis data set
# note:     calls function make.map()

# ROXYGEN Documentation
#' Draw meta-map
#' @description Draw a mapping of a meta-analysis dataset
#' @param meta.ds meta analysis dataset
#' @param compNm name of variable holding the comparator names
#' @param trialNm name of variable holding unique trial identifier
#' @param refNm Reference comparator. Must be a member of meta.ds[,compNm]
#' @param ref.col color of reference node
#' @param comp.col color of comparator nodes
#' @param dist distance for text position
#' @return A graph mapping the dimensions of the meta-analysis dataset
#' @importFrom ellipse ellipse
#' @seealso \code{\link{make.map}} 
#' @export
#' @examples
#' test = c("Ref", paste("comp", 1:5, sep=""))
#' test.ds = data.frame(drg = rep(test, times=seq(12,2,-2)),
#'                      ref.id = c(rep(1:4, 10),1,1))
#' 
#' names(test.ds)
#' meta.map(test.ds, compNm="drg", trialNm="ref.id", refNm="Ref", dist=1.4)

meta.map = function(meta.ds, compNm, trialNm, refNm, 
                    ref.col=blue[2], comp.col=green[3],dist=1.35){
  # Draw a mapping of the relationships in a meta-analysis database, with the 
  # size of the nodes being proportional to the fraction that the comparators
  # make up database, and line thickness being proportional to the number of two-way
  # comparisons that are represented.
  # meta.ds = meta analysis dataset
  # compNm  = name of variable holding the comparator names
  # trialNm = name of variable holding unique trial identifier
  # refNm   = Reference comparator. Must be a member of meta.ds[,compNm]
  # ref.col = color of reference node
  # comp.col = color of comparator nodes
  # dist    = distance for text position

  #if("ellipse" %nin% installed.packages()[, "Package"])
  #  install.packages("ellipse")
  #require(ellipse)
  
  trials = meta.ds[!duplicated(paste(meta.ds[,trialNm], meta.ds[,compNm])),]

  # Find the number of trials for each comparator
  tabl = data.frame(table(Drug=trials[,compNm]))
  ref = tabl[grepl(refNm, tabl$Drug),]
  comps = tabl[!grepl(refNm, tabl$Drug),]
  
  ncat = nrow(comps)
  # find radians
  ints = 2*pi*(0:(ncat-1)/ncat) + pi/2
  #ints = 2*pi*(1:ncat/ncat + 1/(ncat+1)) 
  
  # Find position of the nodes around a circle or radius 10
  comps$pos.x = 10*cos(ints)
  comps$pos.y = 10*sin(ints)
  
  combo.m = make.map(ds=meta.ds, cNm=compNm, trNm=trialNm)
  
  # find x-coordinates
  comp.map = data.frame(table(comp=combo.m$comp))
  comp.map$comp1 = sub("([a-z,A-Z]) .*", "\\1", as.character(comp.map$comp))
  comp.map$comp2 = sub(".* ([a-z,A-Z])", "\\1", as.character(comp.map$comp))
  comp.map$c1x = comp.map$c1y = comp.map$c2x = comp.map$c2y = rep(0, nrow(comp.map))
  comp.map$lwds = rep(1, nrow(comp.map))
  
  # Assign positions for lines and line thicknesses
  for(ii in 1:lunique(comps$Drug)){
    comp.map$c1x[comp.map$comp1==comps$Drug[ii]] = comps$pos.x[ii]
    comp.map$c2x[comp.map$comp2==comps$Drug[ii]] = comps$pos.x[ii]
    comp.map$c1y[comp.map$comp1==comps$Drug[ii]] = comps$pos.y[ii]
    comp.map$c2y[comp.map$comp2==comps$Drug[ii]] = comps$pos.y[ii]
    comp.map$lwds = 30*comp.map$Freq/max(comp.map$Freq)
  }
  
  # make the plot space
  graphics::plot(0,0, 
       xlim=c(-15, 15),
       ylim=c(-15,15),
       type = 'n',
       xlab = " ",
       ylab = " ",
       axes=FALSE)
  # Draw the segments connecting the nodes
  graphics::segments(x0=comp.map$c1x, y0=comp.map$c1y, x1=comp.map$c2x, y1=comp.map$c2y, lwd=comp.map$lwds, col=gray[5])

  # Draw the Reference at 0,0
  ell = ellipse(0, scale=1.2*c(1,0.65),centre = c(0,0))
  graphics::polygon(ell, col=ref.col)
  
  # Draw the comparator nodes
  for(i in 1:nrow(comps)){
    ell = ellipse(0, scale=1.2*sqrt(comps$Freq[i]/ref$Freq)*c(1,.65),centre = c(comps$pos.x[i],comps$pos.y[i]))
    graphics::polygon(ell, col=comp.col)
  }
  graphics::text(0,0, labels=paste(ref$Drug, "\n", ref$Freq, sep=""), offset=-1, pos=3)
  # text(comps$pos.x, comps$pos.y, labels=comps$Freq, pos=3, offset=-0.25, cex=0.7)
  graphics::text(dist*comps$pos.x, dist*comps$pos.y, labels=paste(comps$Drug, "\n", comps$Freq, sep=""),
       pos=3, offset=-0.25)
}  

