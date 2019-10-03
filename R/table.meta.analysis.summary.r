# name:     table.meta.analysis.summary
# purpose:  creates a summary of a meta-analysis dataset
# input:    meta-analysis dataset
# output:   summary table
# note:     

# ROXYGEN Documentation
#' Summarize meta-analysis dataset
#' @description Creates a summary of a meta-analysis dataset. The dataset needs specific column names (see Notes).
#' @param data meta.analysis dataset
#' @param cov.cols character vector with covariate columns 
#' @param cov.cat character vector indicating whether each covariate is continuous or categorical
#' @note Standard column names are 
#' reference	reference number or label to uniquely identify each public literature article/source
#'		study		study identifier or label
#'		drug		drug name in a given arm
#'		dose		dose in a given arm (could be total daily, or some other summary figure)
#'		time		observation time
#'		n		sample size at each observation time
#'		Each unique study+drug+dose = one arm
#'  	
#' 	If there are multiple doses of a drug in a study, computes the AVERAGE starting size
#'	Covariate columns (both continous and categorical) can be specified in the "cov.cols" argument (a vector).
#'	Whether each covariate is continous or categorical is specified in the "cov.cat" argument.
#' @return Summarized meta-analysis dataset
#' @export
#' @importFrom Hmisc summarize
#' @examples 
#' meta.analysis.ds = read.csv(file.path(getOption("qpExampleDir")
#'  , "../Excel/meta.analysis.ds.csv"),stringsAsFactors = FALSE)
#' table.meta.analysis.summary(meta.analysis.ds
#'  , cov.cols=c("age","fem.pct","dis.score.bl","drug.ir")
#'  , cov.cat=c(FALSE,FALSE,FALSE,TRUE)
#' )

table.meta.analysis.summary = function(data, cov.cols=NULL, cov.cat=NULL)
{
  ##  Each unique study+drug+dose = one arm
  ##
  ##	
  ##
  
  #require(Hmisc)
  
  temp 		= data[order(data$time),]
  temp		= temp[!duplicated(paste(temp$study, temp$drug, temp$dose)),]	# ensure we get *starting* sample sizes
  
  n.tab 	= Hmisc::summarize(temp$n
                            , list(drug=temp$drug, study=temp$study)
                            , function(x) round(mean(x))
                            , stat.name = "n.avg"
  )
  
  doses.tab 	= Hmisc::summarize(data$dose
                                , list(drug=data$drug, study=data$study)
                                , function(x) paste(sort(unique(round(x,0))), collapse=", ")
                                , stat.name = "doses"
  )
  
  times.tab 	= Hmisc::summarize(data$time
                                , list(drug=data$drug, study=data$study)
                                , function(x) paste(sort(unique(round(x,0))), collapse=", ")
                                , stat.name = "times"
  )
  
  refs.tab 	= Hmisc::summarize(data$reference
                               , list(drug=data$drug, study=data$study)
                               , function(x) paste(sort(unique(round(x,0))), collapse=", ")
                               , stat.name = "references"
  )
  
  if (!is.null(cov.cols)) {
    cov.tab = n.tab[,c("drug","study")] 
    for (i in 1:length(cov.cols)) {
      # i = 1
      this.cov 		= cov.cols[i]
      this.cov.cat 	= cov.cat[i]
      if (this.cov.cat) {
        this.cov.tab	= Hmisc::summarize(temp[,this.cov], list(drug=temp$drug, study=temp$study),
                                        function(x) paste(sort(unique(x)), collapse=", "), stat.name=this.cov)
      }
      else {
        this.cov.tab	= Hmisc::summarize(temp[,this.cov], list(drug=temp$drug, study=temp$study),
                                        function(x) round(mean(x)), stat.name=this.cov)
      }
      cov.tab[,this.cov] = as.vector(this.cov.tab[,this.cov])
    }
  } 
  
  result 			= doses.tab
  result[,"times"] 		= times.tab[,"times"]
  result[,"n.avg"] 		= n.tab[,"n.avg"]
  result[,"references"]	= refs.tab[,"references"]
  if (!is.null(cov.cols)) result = cbind(result,cov.tab[,cov.cols])
  
  return(result)
} # end function call

