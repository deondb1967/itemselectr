# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Reverse code items
#' 
#' Reflects the scoring of items
#' 
#' @param x Data frame containing items, some of which may need to be reverse coded
#' @param min.score Minimum possible score on the items
#' @param max.score Maximum possible score on the items
#' @param items A vector of item names or column numbers of items that need to be reverse coded
#' @return A data frame
#' @examples
#' library(psychTools)
#' data(bfi)
#' 
#' ## Perform principal components analysis to observe the signs of the component loadings
#' psych::pca(bfi[1:25], 5)
#' items <- c("A1", "C4", "C5", "E1", "E2", "O2", "O5")
#' bfi.new <- reverse.code(bfi[1:25], min.score = 1, max.score = 6, items)
#' psych::pca(bfi.new[1:25], 5)
#' @export
reverse.code <- function (x, min.score, max.score, items) {
  scores = min.score:max.score
  x[, items][!is.na(x[, items])] <- max(scores) - x[, items][!is.na(x[, items])] + min(scores)
  return(x)
}

