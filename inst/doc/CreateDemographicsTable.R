## ---- echo=F, results='hide',message=F,warning=F-------------------------
library(qpToolkit)

## ---------------------------------------------------------------------------------------------------------------------------------------------------

pkpdData = example.pkpdData()

options(width = 150)
ok = duplicated(pkpdData$id) == F
head(pkpdData[ok, ])

## ---- echo = T--------------------------------------------------------------------------------------------------------------------------------------
conDataFun1(pkpdData$wt[ok], 3)
conDataFun2(pkpdData$wt[ok], 3)
catDataFun(pkpdData$sex[ok])

## ---- echo = T--------------------------------------------------------------------------------------------------------------------------------------
tabStats(pkpdData$sex[ok], list(dose = pkpdData$dose[ok]))
tabStats(x=pkpdData$wt, BY = list(dose = pkpdData$dose))

## ---- echo = T--------------------------------------------------------------------------------------------------------------------------------------
tabSummarize(formula = dose ~ race + wt + bmi + sex, data = pkpdData[ok, ], nSignif = 3)

## ---- echo = T--------------------------------------------------------------------------------------------------------------------------------------
pkpdData$trt.o = ordered(pkpdData$trt, levels = unique(pkpdData$trt))
demoTable = tabSummarize(formula = trt.o  ~ race + wt + bmi + sex, data = pkpdData[ok,]
                         , nSignif = 3
                         , extra.blank.line = TRUE)
print(demoTable)

## ----format the demoTable, echo = T, results="asis"-------------------------------------------------------------------------------------------------
demoTable.formatted = format.demoTable(demoTable, formula = trt.o  ~ race + wt + bmi + sex)
demoTable.xtable = xtable(demoTable.formatted
                          , caption = "Demographics Table"
)
print(demoTable.xtable
      , sanitize.text.function = identity
      , include.rownames = F
      , type = "html"
)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
full.names

