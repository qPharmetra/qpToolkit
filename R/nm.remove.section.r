# ROXYGEN Documentation
#' Extract control stream sections
#' @description Function to read NONMEM control stream and extract specific sections
#' @param secNames character vector with sections to remove
#' @param  ctl character vector of scanned control stream
#' @return A list with as many (named) element as there are $.... elements in the control stream
#' @export
#' @note This function is not intended to be called alone, but is called in the wrapper function \code{nm.parse.control.stream}
#' @seealso \code{\link{nm.parse.control.stream}}
#' @examples
#' nm.remove.section("EST"
#'    , read.mod("example1", path = getOption("qpExampleDir"), file.ext = ".ctl")
#'    )[[2]]
#'
nm.remove.section <- function(secNames, ctl){
    # remove a section of a control stream
    # secNames = character vector containing the names of the sections to remove
    # ctl = character vector containing the NONMEM control stream
    #
    # output is a list -- ctl is the edited control stream
    # savSecs is a list containing the sections that were removed

    ## check if they are there
    check.if.they.are.there = sapply(1:length(secNames), function(i, secNames,ctl){
      str = paste("\\$", secNames[i], sep = "")
      length(grep(str, ctl))>0
    }, secNames = secNames, ctl = ctl)
    secNames = secNames[check.if.they.are.there]
    saveSecs = vector("list", length(secNames))

    LEN = length(secNames)
    for(i in 1:LEN){ #i = 9 ;LEN = 7
      str = paste("\\$", secNames[i], sep = "")
      nsec = length(grep(str, ctl))    # likely more than one section of a particular type
      names(saveSecs)[i] = secNames[i]
      saveSecs[[i]] = vector("list", nsec)
      # Identify and remove the next section containing str
      for(j in 1:nsec){ #j = 2
        if(any(grepl(str,ctl))){
          # Identify all sections
          secs = data.frame(sec = ctl[grep("\\$", ctl)], ind = grep("\\$", ctl))
          secs$nextS = c(secs$ind[2:nrow(secs)], length(ctl))
          thisSec = grep(str, secs$sec)[1]
          startS = secs$ind[thisSec]
          endS = ifelse(secs$nextS[thisSec] == length(ctl), length(ctl),
                        secs$nextS[thisSec]-1)
          saveSecs[[i]][[j]] = ctl[startS:endS]
          if(startS<endS) ctl = ctl[-(startS:endS)] else ctl = ctl[-startS]
        }
      }
    }
    return(list(ctl = ctl, saveSecs = saveSecs))
  }
