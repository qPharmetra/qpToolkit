globalVariables('EVID')
#' qPharmetra examples for plot, VPC and NONMEM dataset
#' @description examples for qPharmetra style VPC 
#and GOF plots and a NONMEM data set
#' @family qpexamples
#' @export
#' @examples
#' example.xpose.VPC()

example.xpose.VPC = function()
{
   cat("xpose.VPC(file.path(getOption('nmDir'),'vpc1/vpc_results.csv')
       , file.path(getOption('nmDir'),'vpc1/vpctab1')
       , logy=TRUE 
       , by='STRT'
       , col=grey(0.4),  cex = 1
       , PI.ci.area.smooth = TRUE
       , PI.real.med.col = PI.real.med.col
       , PI.real.down.col = PI.real.down.col
       , PI.real.up.col = PI.real.up.col
       , PI.ci.down.arcol = PI.ci.down.arcol
       , PI.ci.up.arcol = PI.ci.up.arcol
   )")
}
#' 
#' Examples for qPharmetra style CWRES plot
#and a NONMEM data set
#' @family qpexamples
#' @export
#' @examples
#' example.CWRES.plot()

example.CWRES.plot = function()
{
   cat("xyplot(CWRES ~ value | variable
             , data = subset(
                           reshape2::melt(
                                 get.xpose.tables('example1', getOption('qpExampleDir')
                                 )
                        , measure.vars=c('PRED','TIME')
                      ), EVID == 0),
             , panel = panel.cwres
             , xlab = list('Time after Dose',cex=1.25),
             , ylab = list('Conditional weighted residuals', cex = 1.25),
             , aspect = 1,
             , as.table = TRUE
   )"
   )
}

#' Examples for qPharmetra style NONMEM data set
#' @param ID number of subjects
#' @param TIME sequence of times
#' @param DOSE sequence of dose amounts
#' @param \dots ignored
#' @family qpexamples
#' @export
#' @examples
#' library(dplyr)
#' library(magrittr)
#' nmData = example.NONMEM.dataset()
#' nmData %>% group_by(DOSE) %>% summarise(nID=lunique(ID), nObs = length(DV))
#' pkpdData = example.pkpdData()
#' tbl_df(pkpdData)

example.NONMEM.dataset = function(ID=3, TIME=seq(0,24,2), DOSE=c(1,2.5,10), ...)
{
   ## observations
   nmobs = expand.grid(ID = seq(ID*length(DOSE))
                       , TIME = TIME) %>%
      mutate(AMT = 0
             , EVID = 0
             , DV = 0
   ) %>%
      arrange(ID,TIME) 
   nmobs$DOSE = rep(DOSE, ea = ID*length(TIME))
   
   ## doses
   nmdose = subset(nmobs, TIME == TIME[1], select = c(ID,DOSE)) %>% 
      mutate(AMT = DOSE
          , TIME = 0
          , EVID = 1
          , DV = 0
      )

   ## bind NONMEM dataset and sort
   nmdata = bind_rows(nmobs, nmdose)%>%
      arrange(ID,DOSE,TIME,-EVID)
   
   return(nmdata)
}

#' Examples for qPharmetra style PKPD data set
#' 
#' @family qpexamples
#' @export
#' @examples
#' library(dplyr)
#' pkpdData = example.pkpdData()
#' tbl_df(pkpdData)

example.pkpdData = function(){
   set.seed(1234)
   nsub=32
   pkpdData = list(NULL)
   dose = c(0,100,250,500)
   for(i in 1:length(dose))pkpdData[[i]] = expand.grid(id = paste(dose[i],1:nsub), dose = dose[i])
   pkpdData = do.call("rbind", pkpdData)
   pkpdData$id = as.integer(as.factor(pkpdData$id))
   lunique(pkpdData$id)
   pkpdData$age = round(rnorm.by.id(pkpdData$id, mean = 50, sd = 10), 0)
   pkpdData$age[pkpdData$age<20] = 20
   pkpdData$age[pkpdData$age>80] = 80
   pkpdData$wt = round(rnorm.by.id(pkpdData$id, mean = 75, sd = 10), 0)
   pkpdData$ht = sample.by.id(pkpdData$id, 158:200, TRUE)
   pkpdData$bmi = round(pkpdData$wt / (pkpdData$ht/100) ^2, 1)
   pkpdData$sex = sample.by.id(pkpdData$id, c("M","F"), TRUE)
   pkpdData$race = sample.by.id(pkpdData$id, 
                                rep(c("Caucasian","Asian","Black","Other"), c(10,3,3,1)), TRUE)
   pkpdData$endpoint = rep("effect", nrow(pkpdData))
   pkpdData$trt = ifelse(pkpdData$dose == 0, "Placebo", paste("drug ",pkpdData$dose, "mg", sep = ""))
   
   xTime = c(0,1,2,3,4,5,6,7, 10, 14, 21, 28, 35, 42) 
   nr = nrow(pkpdData)
   pkpdData = pkpdData[rep((1:nrow(pkpdData)), ea = length(xTime)), ]
   pkpdData$time = rep(xTime, nr)
   
   nr = nrow(pkpdData)
   pkpdData = rbind(pkpdData, pkpdData)
   pkpdData$type = rep(c("PK", "PD"), ea = nr)
   
   ## simulate a response
   pkpdData$cl = 3 * ((pkpdData$wt/stats::median(pkpdData$wt))^0.5) * exp(rnorm.by.id(pkpdData$id, 0, 0.25))
   pkpdData$v = 10 * exp(rnorm.by.id(pkpdData$id, 0, 0.25)) + (pkpdData$sex == "F") * 5
   pkpdData$keo = 0.05 * exp(rnorm.by.id(pkpdData$id, 0, 0.5))
   
   ok = pkpdData$type == "PK"
   pkpdData$value[ok] = unlist(
      lapply(split(pkpdData[ok, ], pkpdData$id[ok]), function(x)
         pk.1comp.1abs(x$dose, x$time,
                       parms = c(cl = x$cl[1], 
                                 v = x$v[1], 
                                 ka = 1)
         ))
   )
   ## add error
   pkpdData$value[pkpdData$type == "PK"] = 
      pkpdData$value[pkpdData$type == "PK"]*(1+rnorm(sum(pkpdData$type == "PK"), 0, 0.05)) +
      rnorm(sum(pkpdData$type == "PK"), 0, 0.02)
   pkpdData$value[pkpdData$type == "PK" & pkpdData$value<0.05] = 0.05
   
   ok = pkpdData$type == "PD"
   pkpdData$value[ok] = unlist(
      lapply(split(pkpdData[ok, ], pkpdData$id[ok]), function(x)
         1 + eff.1comp.1abs(x$dose, x$time,
                            parms = c(cl = x$cl[1], 
                                      v = x$v[1], 
                                      ka = 1, 
                                      keo = x$keo[1])
         ))
   )
   return(pkpdData)
}

