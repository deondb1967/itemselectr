#' Fits all possible combinations of k items from a pool of n items to a single-factor model using parallel processing.
#'
#' The function can be useful in selecting items from a larger pool of items. The function allows for comparisons of all possible combinations of k items from the pool with respect to fit with a single factor model and reliability. The function works best if the number of possible unique combinations of items is less than 5000. It becomes extremely slow if there are more than 5000 possible combinations. This function employs parallel processing to speed up the process, but it remains slow.
#' @param x A data frame containing the items. The name of the data frame must be x and it must be loaded in the global environment before the function is run.
#' @param k The number of items to be selected.
#' @examples
#'# Say we want to select five items from pool of nine stress items
#'# The choose() function shows how many possible unique combinations there are
#'choose(ncol(work_stress[1:9]), 5)
#'out <- best_itemsPar(work_stress[1:9], 5)
#'out
#' @export
best_itemsPar <- function(x, k) {
  # Find the unique combinations of length k in the items of x
  tcombn <- combn(colnames(x), k, simplify = FALSE)  # Using simplify = FALSE for a list

  # Create a cluster for parallel processing
  library(parallel)
  num_cores <- detectCores() - 1  # Leave one core free
  cl <- makeCluster(num_cores)

  # Export the required objects to the cluster
  clusterExport(cl, varlist = "x")

  # Define the myomega function within the cluster
  clusterEvalQ(cl, {
    library(psych)  # Ensure the psych package is loaded on workers
    myomega <- function(selected_cols) {
      omega.out <- suppressWarnings(suppressMessages(psych::omega(x[, selected_cols], nfactors = 1)))
      mymatrix <- matrix(c(
        round(omega.out$omega.tot, 3),
        round(omega.out$alpha, 3),
        round(omega.out$gstat$rms, 3),
        round(omega.out$gstat$CFI, 3),
        round(omega.out$gstat$TLI, 3),
        round(omega.out$gstat$RMSEA[1], 3),
        round(omega.out$gstat$fit.off, 3)
      ), nrow = 1)

      colnames(mymatrix) <- c("Omega", "Alpha", "SRMR", "CFI", "TLI", "RMSEA", "GFI")
      Items <- toString(selected_cols)
      data.frame(mymatrix, Items)
    }
  })

  # Initialize the progress bar
  pb <- txtProgressBar(min = 0, max = length(tcombn), style = 3)

  # Define a wrapper function to update the progress bar
  progress_wrapper <- function(i) {
    selected_cols <- tcombn[[i]]
    result <- myomega(selected_cols)
    setTxtProgressBar(pb, i)  # Update progress bar
    return(result)
  }

  # Use parLapply for parallel processing
  results <- parLapply(cl, seq_along(tcombn), function(i) {
    selected_cols <- tcombn[[i]]
    myomega(selected_cols)
  })

  # Close the progress bar
  close(pb)

  # Stop the cluster
  stopCluster(cl)

  # Combine the results into a data frame
  omegadf <- do.call(rbind, results)
  colnames(omegadf)[8] <- "Items"

  # Order the results
  omegadf[order(omegadf$SRMR, -omegadf$Omega), ]
}

