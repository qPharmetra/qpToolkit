# name:     insert.blanks
# purpose:  create a blank line in a table typically used for legibility improvement when reporting
# input:    a data frame or matrix, position 
# output:   data frame with blank lines inserted
# note:     used to create demographics tables for example

# ROXYGEN Documentation
#' Inserting blank lines in a table
#' @description Create a blank line in a table typically used for legibility improvement when reporting a table.
#' @param tab the table to insert the line into
#' @param lines vector of numeric row positions in \code{tab} to insert a line 
#' @return a table with lines inserted
#' @export insert.blank.line insert.blanks
#' @examples
#' pkpdData = example.pkpdData()
#' tmp = pkpdData[1:3,]
#' tmp
#' insert.blank.line(tmp, 2)

insert.blanks = function(tab, lines){
  # tab is the target table
  # lines are the lines in tab to be followed by a blank line
  # make a blank line
  # it's is used by tabSummarize to enable nice demographics tables
  blank.line = tab[1,]
  blank.line[1,] = rep("~", ncol(tab))
  
  for(i in rev(sort(lines))){#i=15
    temp = tab[(i+1):nrow(tab),]
    tab = rbind(tab[1:i,], blank.line, temp)
  }
  return(tab)
}

## alias
insert.blank.line = insert.blanks
