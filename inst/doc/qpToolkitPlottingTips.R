## ---- echo=F, results='hide',message=F,warning=F-------------------------
library(qpToolkit)

## ------------------------------------------------------------------------
pkpdData = example.pkpdData()

## ------------------------------------------------------------------------
example("panel.thalf")
example("panel.modelfit")
example("panel.cwres")
example("panel.residual")
example("panel.superpose.arrows")
example("panel.superpose.bubble")

## ------------------------------------------------------------------------
example("plot.tornado")

## ------------------------------------------------------------------------
example.xpose.VPC()
library(xpose4)
xpose.VPC(file.path(getOption('qpExampleDir'),'vpc_final_strt/vpc_results.csv')
       , file.path(getOption('qpExampleDir'),'vpc_final_strt/vpctab')
       , logy=T 
       , by='STRT'
       , col=grey(0.4),  cex = 1
       , PI.ci.area.smooth = T
       , PI.ci.med.arcol = PI.ci.med.arcol
       , PI.real.med.col = PI.real.med.col
       , PI.real.down.col = PI.real.down.col
       , PI.real.up.col = PI.real.up.col
       , PI.ci.down.arcol = PI.ci.down.arcol
       , PI.ci.up.arcol = PI.ci.up.arcol
   )

## ------------------------------------------------------------------------
vpc = nm.read.vpc(file.path(getOption('qpExampleDir'),'vpc_final_strt'), PI.ci.area.smooth=T)

ggvpc_xpose(vpc, point.size = 2) +
   facet_grid(~strata) +
   scale_y_log10()

ggvpc_standard(vpc, point.size = 2) +
   facet_grid(~strata) +
    scale_y_log10()

## ------------------------------------------------------------------------
vpc = nm.read.vpc(file.path(getOption('qpExampleDir'),'vpc_final_strt'), PI.ci.area.smooth=T)
vpc = lapply(vpc, function(x) expand.data(x, values=1:2,name = "pat.class"))

vpc$obs$TIME[vpc$obs$pat.class==1] = vpc$obs$TIME[vpc$obs$pat.class==1]/0.5
vpc$res$xCov[vpc$res$pat.class==1] = vpc$res$xCov[vpc$res$pat.class==1]/0.5
vpc$vpc$xCovm[vpc$vpc$pat.class==1] = vpc$vpc$xCovm[vpc$vpc$pat.class==1]/0.5

vpc$obs$TIME[vpc$obs$pat.class==2] = vpc$obs$TIME[vpc$obs$pat.class==2]/1.5
vpc$res$xCov[vpc$res$pat.class==2] = vpc$res$xCov[vpc$res$pat.class==2]/1.5
vpc$vpc$xCovm[vpc$vpc$pat.class==2] = vpc$vpc$xCovm[vpc$vpc$pat.class==2]/1.5

vpc = lapply(vpc, function(x) {
   x$pat.class = swap(x$pat.class, sunique(x$pat.class), Cs(Obese,Normal_BodyWeight))
   return(x)}
   )

vpc = lapply(vpc, function(x) {
   x$strata = swap(x$strata, sunique(x$strata), Cs(study1,study2))
   return(x)}
   )

ggvpc_xpose(vpc, point.size = 2) +
   facet_grid(pat.class~strata) +
   scale_y_log10()

## ------------------------------------------------------------------------
example("histogram.bootstrap")

## ------------------------------------------------------------------------
example("bootstrap.ParTab")

