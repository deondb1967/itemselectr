# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Selecting items based on Gulliksen's item reliability index
#'
#' Iteratively removes item with the lowest item reliability index from a pool of items. At each step this yields a test with the largest possible variance.
#' @param x A data frame that contains the items to be analysed
#' @param nitems The number of items to retain. This value should never be less than three.
#' @return At each step the number of items that have been removed, the name of the last item that has been removed, omega, the average item score, and the standard deviation of the test.
#' @examples
#' Select the combination of k items that will maximise the test variance
#' item.selection(work_stress[1:9], 5)
#' @export
item.selection <- function(x, nitems) {
  yyy <- psych::alpha(x, check.keys = TRUE)$item.stats
  rel.index <- yyy$raw.r*yyy$sd
  names(rel.index) <- colnames(x)
  sorted.rel.index <- sort(rel.index)
  item.stats <- cbind(yyy, rel.index)
  aa <- list(rel.index = rel.index, sorted.rel.index = sorted.rel.index, item.stats = item.stats)


##item.reliability.index(x)
items.wanted = nitems

Total.score <- rowMeans(x)
Total.mean  <- mean(Total.score, na.rm = TRUE)
Total.sd    <- sd(Total.score, na.rm = TRUE)

## Data frame that tracks omega as you remove items iteratively
StepOmega         <- as.data.frame(c(0, ncol(x),"None", MBESS::ci.reliability(x, type = "hierarchical")[1],
                                     psych::alpha(x)$total[1], psych::alpha(x)$total[4], Total.mean, Total.sd))
names(StepOmega)  <- c('Items.out','Items.in','Item.removed', 'Omega', 'Alpha', 'Average r', 'Mean', 'SD')


for (i in 1:(ncol(x) - items.wanted)) {
  low.iri  <- names(sorted.rel.index[i])
  item.out <- names(x) %in% c(low.iri)

  ### Remove item with the lowest information from data frame
  x <- x[!item.out]

  # Calculate omega with MBESS package
  rel             <- MBESS::ci.reliability(x, type = "hierarchical")$est
  myalpha         <- psych::alpha(x)$total[1]
  ave.r            <- psych::alpha(x)$total[4]
  # Calculate total score
  Total.score <- rowMeans(x)
  Total.mean  <- mean(Total.score, na.rm = TRUE)
  Total.sd    <- sd(Total.score, na.rm = TRUE)


  # Store the step, name of item removed and omega in a data frame
      LoopOut         <- data.frame(i, ncol(x),low.iri, rel, myalpha, ave.r, Total.mean, Total.sd)
      names(LoopOut ) <- c('Items.out', 'Items.in', 'Item.removed','Omega', 'Alpha','Average r','Mean', 'SD')

      StepOmega       <-  rbind(StepOmega,LoopOut)
}
results   <- list(Steps          = StepOmega,
                  Items.in.scale = names(x),
                  Omega          = suppressMessages(MBESS::ci.reliability(x, type = "hierarchical")[1]),
                  Alpha          = psych::alpha(x)$total[1])
print(results)
}

