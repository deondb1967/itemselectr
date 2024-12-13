#' Large factor analysis residuals
#'
#' Identifying large residuals in a matrix of factor analysis residuals
#'
#' @param x A square matrix of residuals of a factor analysis
#' @param upper Upper limit for acceptable residuals (defaults to 0.08)
#' @param lower Lower limit for acceptable residuals (defaults to -0.)
#' @return Statistics relating to the residuals
#' @examples
#' # Find the residuals of a factor analysis
#' myresiduals <- psych::fa(work_stress[1:9], nfactors = 1)$resid
#'
#' # Identify the pairs of items with the largest residuals
#' myoutput <- large.residuals(x = myresiduals, upper = -.01, lower = .01)
#' myoutput
#' @export
large.residuals <- function(x, upper = .08, lower = -.08) {
  #### Set the values on the diagonal of the residual matrix to zero
  diag(x) <- NA

  #Print only the lower triangle of the residual matrix
  upper.tri(x, diag = TRUE)
  x[upper.tri(x)] <- NA

  #Store the lower triangle matrix as a vector and remove NAs
  myvec <- as.vector(x)
  myvec <- na.omit(myvec)
  myvec <- as.vector(myvec)

  #Descriptive statistics of the residuals
  median.resid <- median(myvec)
  mean.resid   <- mean(myvec)
  max.resid    <- max(myvec)
  min.resid    <- min(myvec)
  my.stem <- stem(myvec, scale = 2)
  myvec.sort <- sort(myvec)

  #### Find the coordinates of residuals > or < than specific values
  big.resid <- (which(x  >  upper | x < lower, arr.ind = T))


  # Create an empty data frame with the specified column names
  mydf <- data.frame(Row = character(1),
                     Column = character(1),
                     Cor.residual = numeric(1),
                     stringsAsFactors = FALSE, row.names = NULL)


  ### Find the row and column names of variables in large residuals
   for (kk in 1:nrow(big.resid)) {
   row <- big.resid[kk, 1]
   col <- big.resid[kk, 2]


   rows_cols           <- data.frame(rownames(x)[row], colnames(x)[col], round(x[row,col], 3), row.names = NULL)
   colnames(rows_cols) <- c("Row", "Column", "Cor.residual")
   mydf <- rbind(mydf, rows_cols)
    }


  #### Percentage large residuals
  prop.large.resid <- (length(big.resid)/length(myvec))/2

  ### Print the results
  outcomes <- list(lower.triangle   = round(x, 3),
                   coordinates      = big.resid,
                   items            = mydf[-1, ],
                   sorted.residuals = round(myvec.sort, 3),
                   median.residual  = round(median.resid, 3),
                   mean.residual    = round(mean.resid, 3),
                   max.residual     = round(max.resid, 3),
                   min.residual     = round(min.resid, 3),
                   proportion.large.residuals = round(prop.large.resid, 3),
                   number.big.residuals       = length(big.resid)/2)
  #return(outcomes)

}
