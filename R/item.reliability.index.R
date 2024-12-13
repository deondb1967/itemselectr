#' Gulliksen's item reliability index extended to polytomous items
#'
#' Takes the item-total correlation and multiplies it with the item standard deviation
#' @param x A data frame that contains the items to be analysed
#' @return The item reliability index of each item
#' @examples
#' #' item.reliability.index(work_stress[1:9])
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
