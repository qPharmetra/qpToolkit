
# ROXYGEN Documentation
#' Group-wise sampling from normal distribution 
#' @description Populates 1 sample from normal distribution for each group (id or subject, or trial, or other)
#' @param id vector with group information 
#' @param mean mode of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @return samples of a normal distribution, one for each level of \code{group}
#' @export
#' @seealso \code{\link{sample.by.id}}
#' @examples
#' sam = rnorm.by.id(pkpdData$id, mean = 0, sd = 1)
#' tapply(sam,pkpdData$id, unique) 

rnorm.by.id = function(id, mean = 0, sd = 1)
  {
    len = length(unique(id))
    rnorm(n = len, mean = mean, sd = sd)[tapply(id, id)]
  }



