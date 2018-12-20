## ---- include=F----------------------------------------------------------
library(qpToolkit)
library(plyr)
library(reshape)

# path to examples NONMEM folder
nm.path = file.path(getOption("qpExampleDir"))
#hide the code blocks in the output
opts_chunk$set(echo=F)

## ------------------------------------------------------------------------
data.df = read.csv(file.path(nm.path,"example2.csv"),skip=1)
data.df = rename(data.df,c("E"="EVID","M"="MDV","CONC"="DV"))

## ------------------------------------------------------------------------
ggplot(subset(data.df,EVID==0), aes(x=TIME,y=DV,group=ID)) + geom_line() + 
  scale_y_log10() +
  theme_bw() + labs(x="Time (h)", y="Concentration (ng/ml)")

## ------------------------------------------------------------------------
ggplot(subset(data.df,EVID==0), aes(x=TIME,y=DV,group=ID)) + geom_line(alpha=.2) + 
  theme_bw() + 
  labs(x="Time (h)", y="Concentration (ng/ml)") + scale_y_log10() + 
  stat_summary(aes(group=NULL),fun.data = "mean_cl_normal", colour = "red", geom="smooth")

## ---- eval=F, echo=F-----------------------------------------------------
#  library(GGally)
#  cov.names = Cs(Gender,Age)
#  covs.df = data.df[!duplicated(data.df$ID),cov.names]
#  covs.df$Gender = as.factor(c("M","F")[covs.df$Gender+1])
#  p.cov.corr.png = ggpairs(covs.df, diag=list(continuous="density", discrete="bar"), axisLabels="show")
#  print(p.cov.corr.png)
#  

## ----results='asis'------------------------------------------------------
rr =  read.runrec("AAruninfo.txt",nm.path)
kable(process.runrec(rr,plain=T),row.names=F)

## ------------------------------------------------------------------------
run="example1"
out = get.xpose.tables(run, path = nm.path)
trellis.strip.color()
xyplot(CWRES ~ value | variable
             , data = reshape2::melt(subset(out,EVID==0), measure.vars=Cs(PRED,TIME))
             , panel = panel.cwres
             , xlab = list('Time after Dose',cex=1.25),
             , ylab = list('Conditional weighted residuals', cex = 1.25),
             , aspect = 1,
             , as.table = T
   )

xyplot(Cbind(CONC,PRED,IPRED,EVID) ~ TIME | ID
       , groups = ID
       , data = subset(out, EVID!=1 & ID %in% unique(ID)[1:16])
       , scales = list(x = list(relation = 'free'),y = list(log=10))
       , panel = panel.modelfit
       , logY = T
       , yscale.components = yscale.components.log10
)

## ----covSignals, echo=F--------------------------------------------------
cov = nm.covplot("example2",path=nm.path,covlist=list(cat=c("GNDR"),con=c("AGE"))
                 ,parameters = Cs("CL","V1"), eta.skip=Cs(ETA3,ETA4))

cov$parContVarPlot()
cov$parCatVarPlot()

## ----echo=FALSE----------------------------------------------------------
cov$eta.dens()
cov$eta.splom()
cov$etaContVarPlot()
cov$etaCatVarPlot()

## ---- results='asis', warning=FALSE--------------------------------------

scm.trail=nm.process.scm(file.path(nm.path, "scm_example2"))

cat("\n#### Summary of Steps \n")
kable(scm.trail$summary)

cat("\n#### Final Model \n")
kable(scm.trail$model)

cat("\n#### Model Building Steps \n")

for(i in 1:length(scm.trail$steps))
  {
  cat(paste0("\n", scm.trail$steps[[i]]$label,"\n"))
  print(kable(scm.trail$steps[[i]]$runs[[1]]))
  cat(paste0("\n", scm.trail$steps[[i]]$action,"\n"))
  }

## ------------------------------------------------------------------------
run="example3"
out = get.xpose.tables(run, path = nm.path)
trellis.strip.color()
xyplot(CWRES ~ value | variable
             , data = reshape2::melt(subset(out,EVID==0), measure.vars=Cs(PRED,TIME))
             , panel = panel.cwres
             , xlab = list('Time after Dose',cex=1.25),
             , ylab = list('Conditional weighted residuals', cex = 1.25),
             , aspect = 1,
             , as.table = T
   )

xyplot(Cbind(CONC,PRED,IPRED,EVID) ~ TIME | ID
       , groups = ID
       , data = subset(out, EVID!=1 & ID %in% unique(ID)[1:16])
       , scales = list(x = list(relation = 'free'),y = list(log=10))
       , panel = panel.modelfit
       , logY = T
       , yscale.components = yscale.components.log10
)

## ----result='asis'-------------------------------------------------------
params.df = nm.params.table("example3",path=nm.path)
kable(params.df)

## ----results='asis'------------------------------------------------------
xml.base=nm.extract.xml("example1",nm.path)
xml.final=nm.extract.xml("example3",nm.path)

w.base=c(diag(make.symmetric(xml.base$omega[[5]],triangle="lower")))
         #,diag(make.symmetric(xml.base$sigma[[5]] ,triangle="lower")))
w.final=c(diag(make.symmetric(xml.final$omega[[1]],triangle="lower")))
         #,diag(make.symmetric(xml)))
w.names = c(paste("ETA",1:(length(xml.base$etabar[[1]][[1]]))))
            #,paste("SIGMA",1:(length(xml.base$epsshrink[[1]]))))
var.df = data.frame(Parameter=w.names,Base=w.base,Final=w.final) 
var.df$Explained=1-var.df$Final/var.df$Base
kable(var.df)

## ------------------------------------------------------------------------
eqs=make.eqs(CL=THETA1*(1 + (GNDR==0)*THETA7)*(AGE/33.72)^THETA6*exp(ETA1)
             ,V1=THETA2*(1 + (GNDR==0)*THETA9)*(AGE/33.72)^THETA8*exp(ETA2)
             )
eqs$label=list(CL="Clearance (L/h)", V1="Volume of Central Compartment (L)")


## ------------------------------------------------------------------------
params.l=as.list(setNames(params.df$Estimate,params.df$Parameter))
params.l$ETA1=0
params.l$ETA2=0

## ----echo=F--------------------------------------------------------------

xpose.df = get.xpose.tables("example3",nm.path)
xpose.df = xpose.df[!duplicated(xpose.df$ID),c("GNDR","AGE","CL","V1")]
covs.info = makeCovInfo(xpose.df[,c("GNDR","AGE")],cat.covs = "GNDR")
covs.info$GNDR$label = "Gender (kg)"
covs.info$AGE$label = "Age (Y)"
covs.info$AGE$breaks = c(1:5,(1:8)*10)
covs.info$AGE$center=33.72


## ------------------------------------------------------------------------
tmp = nm.extract.xml("example3",nm.path)
coveffs.l = nm.process.coveffects(eqs = eqs
                                  , covs.info = covs.info
                                  , pars = data.frame(params.l)
                                  , xpose.df = xpose.df
                                  , omega = make.symmetric(
                                     unlist(tmp$omega),triangle = "upper")
)

plot(coveffs.l)

## ---- echo=FALSE, warning=FALSE, message=F-------------------------------
myVPC = nm.read.vpc(path = file.path(nm.path, "vpc_final_strt"), PI.ci.area.smooth = T)
ggvpc_xpose(myVPC, point.size = 2) + scale_y_log10() + facet_wrap(~strata)
ggvpc_standard(myVPC, point.size = 2) + scale_y_log10() + facet_wrap(~strata)

