#' Fits all possible combinations of k items from a pool of n items to a single-factor model.
#'
#' The function can be useful in selecting items from a larger pool of items. The function allows for comparisons of all possible combinations of k items from the pool with respect to fit with a single factor model and reliability. The function works best if the number of possible unique combinations of items is less than 5000. It becomes extremely slow if there are more than 5000 possible combinations.
#' @param x A data frame containing the items.
#' @param k The number of items to be selected.
#' @examples
#'# Say we want to select five items from pool of nine stress items
#'# The choose() function shows how many possible unique combinations there are
#'choose(ncol(work_stress[1:9], 5))
#'out <- best_items(work_stress[1:9], 5)
#'out
#' @export
best_items <- function(x, k) {
    # Find the unique combinations of length k in the items of x
    tcombn <- t(combn(colnames(x), k))

    myomega <- function(x) {
      omega.out <- suppressWarnings(suppressMessages(psych::omega(x, nfactors = 1)))
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
      mydf <- mymatrix
      Items <- data.frame(toString(colnames(x)))
      mydf <- data.frame(mydf, Items)
      mydf
    }

    omegadf <- data.frame(
      Omega = numeric(),
      Alpha = numeric(),
      SRMR = numeric(),
      CFI = numeric(),
      TLI = numeric(),
      RMSEA = numeric(),
      GFI = numeric(),
      Items = character()
    )

    # Initialize the progress bar
    pb <- txtProgressBar(min = 0, max = nrow(tcombn), style = 3)

    # Assuming myomega returns a list or a data frame
    results <- lapply(1:nrow(tcombn), function(i) {
      # Update progress bar
      setTxtProgressBar(pb, i)
      myomega(x[, tcombn[i,]])
    })

    # Close the progress bar
    close(pb)

    # Combine the results into a data frame
    omegadf <- do.call(rbind, results)
    colnames(omegadf)[8] <- "Items"
    omegadf[order(omegadf$SRMR, -omegadf$Omega), ]
  }
