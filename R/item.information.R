# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' McDonald's item information statistic
#' 
#' Calculates item information statistics (McDonald, 1999).
#' 
#' @param x A data frame containing items
#' @return A matrix containing for each item its (a) unstandardized factor loading, (b) square of the loading, (c) unique variance, and (d) item information.
#' @examples
#' library(lordif)
#' data(Anxiety)
#' info <- item.information(Anxiety[4:32])
#' info
#' @export 
item.information <- function(x) {
  myfa <- psych::fa(x, nfactors = 1, covar = TRUE)      
  loadings <- myfa$loadings
  uniquenesses <- as.vector(myfa$uniquenesses)
  information <- loadings^2/uniquenesses
  Item_information <- cbind(loadings, loadings^2, uniquenesses, information)
  colnames(Item_information) <- c("Loadings", "Loadings_squared", "Uniquenesses", "Information")
  Item_information
  Item_information.sorted <- Item_information[order(Item_information[,4]), ]
  results <- list("Item information statistic" = Item_information[,4],
                  "Item information statistic sorted" = Item_information.sorted[,4],
    "Item information" = round(Item_information, 3),
                  "Item information sorted" = round(Item_information.sorted, 3))
  print(results)
}
