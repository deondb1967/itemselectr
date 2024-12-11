# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Performs a differential item functioning analysis of polytomous items using ANCOVA
#' 
#' Differential item functioning analysis of polytomous items via ANCOVA
#' 
#' @param data A data frame containing the items. All the items should be scored in the same direction.
#' @param Trait A vector containing measures of the trait. This may be the total scores across all the items, the mean item scores, factor scores, or IRT person measures.
#' @param Group A factor indicating group membership
#' @examples
#'library(lordif)
#'data(Anxiety)
#'### Perform the dif analysis
#'out <- dif.ancova(Anxiety[4:32], rowSums(Anxiety[4:32]), Anxiety$gender) 
#'out
#' @export 
dif.ancova <- function(data, Trait, Group) {
  myDIFaov    <- lapply(data, function(x) summary(aov(x ~ Trait * Group)))
  myDIFlist   <- lapply(myDIFaov, function(x) x[[1]]$`Pr(>F)`)
  myDIFdf     <- t(data.frame(myDIFlist))
  myDIFdf     <- myDIFdf[, 1:3]

  myesq    <- lapply(data, function(x) effectsize::eta_squared(aov(x ~ Trait * Group), partial = FALSE)[, 2])
  myesq.df <- t(data.frame(myesq))
  
  myDIFmatrix           <- cbind(myesq.df, myDIFdf)
  colnames(myDIFmatrix) <- c("Trait_eta.sq", "Udif_eta.sq", "NUdif_eta.sq", "Trait_p", "Udif_p", "NUdif_p")
  round(myDIFmatrix, 4)
}
