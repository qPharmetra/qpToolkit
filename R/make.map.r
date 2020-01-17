
# ROXYGEN Documentation
#' Meta-analysis data tabulation
#' @description Compose table of all two-way combinations of comparators and references
#' @param ds meta-analysis dataset
#' @param cNm column names of comparators
#' @param trNm column names of reference drugs
#' @return A dataset summarizing the dimensions of the meta-analysis dataset
#' @return Placeholder for return description
#' @importFrom utils combn
#' @seealso \code{\link{meta.map}}
#' @note not intended to be used as standalone. Is called from qP function meta.map()
#' @examples
#' test = c("Ref", paste("comp", 1:5, sep=""))
#' test.ds = data.frame(drg = rep(test, times=seq(12,2,-2)),
#'                      ref.id = c(rep(1:4, 10),1,1))
#'
#' names(test.ds)
#' meta.map(test.ds, compNm="drg", trialNm="ref.id", refNm="Ref", dist=1.4)
make.map <- function(ds, cNm="drug.abbr", trNm="ref.tr"){
  # identify all of the 2-way comparisons among the comparators
  # meta.ds = meta analysis dataset
  # compNm = variable name of the different comparators
  # trialNm = variable name for the trial identifier
  # Create the map of the different comparisons
  combos = tapply(  ds[,cNm], ds[,trNm],
                    function(x) {
                      len = min(c(2, lunique(x)))
                      return(unlist(t(combn(as.character(sunique(x)),len))))
                    })

  # Identify those studies that have at least one comparator.
  sel = unlist(lapply(combos, function(listel){dim(listel)[2]>1} ))
  combo.m = data.frame(do.call("rbind", combos[sel]))
  names(combo.m) = c('comp1', 'comp2')
  combo.m$comp = paste(combo.m$comp1, combo.m$comp2)
  return(combo.m)
  # combo.m is now a table of all two-way comparisons
}
