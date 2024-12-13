#' Inter-item correlations
#'
#' Statistics related to the inter-item correlations of the items
#'
#' @param x Data frame containing items that are all scored in the same direction
#' @return A list containing (a) the inter-item correlation matrix, (b) for each item its average correlation with the remaining items, (c) the squared multiple correlation of each item with the remaining items, and (d) the average inter-item correlation across all the items, and (e) the median inter-item correlation across all the items.
#' @examples
#' cor.item(work_stress[1:9])
#' @export
cor.item <- function(x) {
k <- cor(x, use = "complete.obs")
n <- k
diag(n) <- NA
l <- colMeans(n, na.rm = TRUE)
m <- mean(n, na.rm = TRUE)
o <- median(n, na.rm = TRUE)
ki <- solve(k)
smcs <- 1-(1/diag(ki))
total <- rowMeans(x)
itr <- cor(total, x, use= "complete.obs")


corlist <- list(Correlation.matrix = round(k, 3),
                Average.r = round(m, 3),
                Median.r = round(o, 3),
                Item.average.r = round(l, 3),
                Item.average.r.sorted = round(sort(l), 3),
                Squared.multiple.correlation = round(smcs, 3),
                Squared.multiple.correlation.sorted = round(sort(smcs), 3),
                Item.total.correlation = round(itr[1, ], 3),
                Item.total.correlation.sorted = round(sort(itr[1,]), 3))
print(corlist)
}
