globalVariables(c('V1','V2','V3','V4','V5','V6','V7','V8','V9','Step','step','direction','chosen','Chosen','pvalue','deltadf','significant'))
# ROXYGEN Documentation
#' Parsing SCM output
#' @description Read and process SCM output file such that the output can be directly copied into Microsoft products and LaTeX typesetting.
#' @param path full directory path where scmlog files reside
#' @param ... ignored
#' @return data.frame
#' @export
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_at
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' library(encode)
#' library(magrittr)
#' library(dplyr)
#' library(knitr)
#' path = file.path(getOption("qpExampleDir"),"scm_example2")
#' nm.process.scm(path)
#' path %>%
#'   nm.process.scm %>% 
#'   filter(chosen == 1) %>%                      # summary
#'   select(step, model, direction, pvalue) %>%
#'   formalize %>%                                # labels as colum names
#'   decode %>%                                   # substitute guide values 
#'   kable
#'  # model:
#'  path %>% nm.process.scm %>% filter(chosen == 1) %>%
#'  select(model)

nm.process.scm <- function (path, ...) {
  stopifnot(length(path) == 1)
  stopifnot(is.character(path))
  
  x <- readLines(file.path(path, 'scmlog.txt'))
  
  # x[grepl('V1AGE-5', scm)]
  #  "V1AGE-5          PVAL  -9746.45167-10176.09933            429.64766      6.63490    1        YES!  0.00e+000"
  
  x <- gsub('([0-9])-([0-9])','\\1 -\\2',x)
  
  updatetext <- function(x) {
    x <- gsub("TEST OFV (DROP)", " TestOFV ", x, fixed = TRUE)
    x <- gsub("BASE OFV", " BaseOFV", x)
    x <- gsub("NEW OFV", " NewOFV", x)
    x <- gsub(">", " ", x)
    return(x)
  }
  
  x <- updatetext(x)
  
  skip <- c("Model", "--------------------", "CRITERION",
            "BASE_MODEL_OFV", "CHOSEN_MODEL_OFV",
            "Relations", "--------------------", "MODEL")
  
  `%nin%` <- Negate(`%in%`)
  
  x <- read.table(textConnection(x), fill = TRUE)
  x %<>% filter(V9 !="" | V8!="" | V7!="") 
  x %<>% filter(V1 %nin% skip) 
  x %<>% mutate(Chosen = ifelse(grepl("-", V8), ifelse(grepl("e", V8), NA, V8), NA)) 
  x %<>% mutate(V9 = paste(V8, V9, sep=" "))
  x %<>% mutate(V8 = ifelse(grepl("YES!", V8), "YES", ""))
  x %<>% mutate(row = as.numeric(rownames(.)))
  x %<>% mutate(Step = ifelse(
      grepl("forward", V6), 
      "Forward",
      ifelse(
        grepl(
          "backward", V6), 
          "Backward",
          ifelse(
            row==1, 
            "Forward",
            ifelse(
              grepl("inside", V7), 
              "Backward", 
              NA
            )
          )
        )
      )
    )
  x %<>% mutate_at(vars(V3:V6),function(x)suppressWarnings(as.numeric(x)))
  x %<>% rename(direction = Step)
  x %<>% mutate(step = cumsum(!is.na(direction)))
  x %<>% mutate(direction = locf(direction))
  x %<>% mutate(Chosen = rev(locf(rev(Chosen))))
  x %<>% mutate(Chosen = as.integer(V1 == sub('-','',Chosen)))
  
  x %<>% mutate(V1 = gsub("Parameter-covariate", "", V1, fixed = TRUE))
  x %<>% mutate(V1 = gsub("Forward", "", V1))
  x %<>% mutate(V7 = gsub("step:", "", V7, fixed = TRUE))
  x %<>% mutate(V7 = gsub("inside", "", V7))
  x %<>% filter(V1 != '')
  if(!all(x$V2 == 'PVAL'))warning('expecting PVAL; seeing ',paste(unique(x$V2),collapse = ', '))
  x %<>% select(-V2)
  
  x %<>% select(step, V1, V3, V4, V5, V6, V7, V8, V9, Chosen, direction)
  names(x) <- c("step", "model", "ofvbase", "ofvtest", "dofv", "goal", "deltadf", "significant", "pvalue", "chosen", "direction")
  text2decimal <- function (x) as.numeric(sub("^[^0-9.+-]*([0-9.eE+-]+).*$", "\\1", as.character(x)))
  x %<>% mutate(pvalue = text2decimal(pvalue))
  #x %<>% mutate(pvalue = readr::parse_number(pvalue)) 
  x %<>% mutate(deltadf = suppressWarnings(as.numeric(deltadf))) 
  # x %<>% mutate(
  #   pvalue = ifelse(
  #     as.numeric(pvalue) > 0.9999, 
  #     "> 0.9999",
  #     ifelse(
  #       as.numeric(pvalue)<0.0001, 
  #       "< 0.0001",
  #       as.character(signif(as.numeric(pvalue), digits = digits))
  #     )
  #   )
  # )
  x %<>% mutate(significant = ifelse(significant == 'YES',1,0))
  x %<>% mutate(direction = sub('Forward','+', direction))
  x %<>% mutate(direction = sub('Backward','-', direction))
                              
  
  attr(x$step, 'label') <- 'Step'
  attr(x$model, 'label') <- 'Model'
  attr(x$ofvbase, 'label') <- 'Base Objective Function Value'
  attr(x$ofvtest, 'label') <- 'Test Objective Function Value'
  attr(x$dofv, 'label') <- 'Delta Objective Function Value'
  attr(x$goal, 'label') <- 'Reference Delta Objective Function Value'
  attr(x$deltadf, 'label') <- 'Delta Degrees of Freedom'
  attr(x$significant, 'label') <- 'Significant'
  attr(x$significant, 'guide') <- '//0/no//1/yes//'
  attr(x$pvalue, 'label') <- 'Test P-value'
  attr(x$chosen, 'label') <- 'Effect Was Selected'
  attr(x$chosen, 'guide') <- '//0/no//1/yes//'
  attr(x$direction, 'label') <- 'Search Type'
  attr(x$direction, 'guide') <- '//+/Addition//-/Deletion//'
  class(x) <- union('decorated', class(x))
  class(x) <- union('scm', class(x))
  return(x)
}
