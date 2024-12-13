#' Create norms for psychological or educational tests
#'
#' Creates percentiles, McCall T-scores (M = 50, SD = 10) and stanines (M = 5, SD = 2)
#'
#' @param xsum A vector or raw total scores for a test (no missing data)
#' @examples
#' # Create norms for the work_stress scale
#' Stotal <- rowSums(work_stress[1:9])
#' make.norms(Stotal)
#'
#' @export
make.norms <- function(xsum) {
  ### Import the example data. A Windows Explorer window should open on your toolbar
  X <- as.matrix(xsum)

  ### Convert the total scores to norms
  freq <- table(X)
  cumsum(freq)

  freq_table <- cbind(as.numeric(names(freq)),
                      freq,
                      cumsum(freq))

  colnames(freq_table) <- c("Scores", "Freq", "CumFreq")
  freq_table

  scores <- min(X):max(X)
  percentiles <- numeric(length(scores)) # output vector

  for (i in 1:length(scores)) {
    score <- scores[i]
    freq_row <- which(freq_table[, 1] == score)

    if (length(freq_row) == 0) {
      num_below <- 0
    } else {
      num_below <- freq_table[freq_row, 3] - freq_table[freq_row, 2] / 2
    }

    percentiles[i] <- num_below / length(X) * 100
  }

  percentiles <- ifelse(is.na(percentiles), 0, percentiles)

  cbind(scores, percentiles)

  ### T-scores
  p <- numeric(length(X))
  for (i in 1:length(X)) {
    p[i] <- percentiles[which(X[i] == scores)]
  }
  z_score <- qnorm(p / 100)
  T_score <- z_score * 10 + 50

  hist(T_score)

  my_stanine <- function(percent) {
    ranges <- c(0, 4, 11, 23, 40, 60, 77, 89, 96, 100)
    stanine <- NA
    for (i in 1:9) {
      if (percent > ranges[i] && percent <= ranges[i + 1]) {
        stanine <- i
      }
    }
    stanine
  }

  stanine <- numeric(length(p))
  for (i in 1:length(p)) {
    stanine[i] <- my_stanine(p[i])
  }

  result <- cbind(X, p, T_score, stanine)
  colnames(result) <- c("Raw_Score", "Percentile", "T_Score", "Stanine")
  result

  mynorms <- result[order(result[, 1], decreasing = FALSE), ]
  mynorms <- data.frame(mynorms)
  mynorms <- mynorms[!duplicated(mynorms$Raw_Score), ]
  print(mynorms, row.names = FALSE, digits = 2)
}
