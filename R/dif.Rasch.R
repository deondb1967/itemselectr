#' Detection of statistically significant differential item functioning via ANCOVA of standardized Rasch residuals
#'
#'
#' @param std.residuals A matrix or data frame containing the standardized residuals of a Rasch analysis
#' @param Theta A vector containing thetas (person measures)
#' @param Group A vector (factor) containing group membership
#' @return DIF results
#' @examples
#' library(TAM)
#' library(lordif)
#' Anxiety <- Anxiety - 1
#'
#' ## Fit the partial credit model
#' myRasch <- Simple.Rasch(Anxiety[ , 4:32], "PCM2")
#'
#' ## Perform the DIF analysis using the standardized residuals, Rasch person measure and group as input
#' dif.Rasch(myRasch$std.residuals, myRasch$thetas[,1], Anxiety$gender)
#' @export
dif.Rasch <- function(std.residuals, Theta, Group) {
  std.residuals <- data.frame(std.residuals)
  myDIFrasch  <- lapply(std.residuals, function(x) summary(aov(x ~ Theta * Group)))
  myDIFlist   <- lapply(myDIFrasch, function(x) x[[1]]$`Pr(>F)`)
  myDIFdf     <- t(data.frame(myDIFlist))
  myDIFdf     <- myDIFdf[, 1:3]
  colnames(myDIFdf) <- c("Trait", "Uniform", "Non_uniform")
  round(myDIFdf, 4)
}
