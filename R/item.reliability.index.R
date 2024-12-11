# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Gulliksen's item reliability index extended to polytomous items
#'
#' Takes the item-total correlation and multiplies it with the item standard deviation
#' @param x A data frame that contains the items to be analysed
#' @return The item reliability index of each item
#' @examples
#' #Calculate item reliability indexes for the items of the Neuroticism Scale of the bfi data set
#' library(psychTools)
#' data(bfi)
#' item.reliability.index(bfi[16:20])
#' @export 
item.reliability.index <- function(x) {
  zzz <- psych::alpha(x, check.keys = TRUE)
  yyy <- zzz$item.stats
  rel.index <- yyy$raw.r*yyy$sd
  names(rel.index) <- colnames(x)
  sorted.rel.index <- sort(rel.index)
  item.stats <- cbind(zzz$item.stats, rel.index)
  return(list(rel.index = rel.index, sorted.rel.index = sorted.rel.index, item.stats = item.stats))
}
