#' Performs a differential item functioning analysis of polytomous items using ANCOVA
#'
#' Differential item functioning analysis of polytomous items via ANCOVA
#'
#' @param x A data frame containing the items. All the items should be scored in the same direction.
#' @param Group A factor indicating group membership
#' @param Trait A vector containing measures of the trait. This may be the total scores across all the items, the mean item scores, factor scores, or IRT person measures.
#' @examples
#'
#'# Differential item functioning across gender
#'## Calculate the Trait score
#'Trait <- rowSums(work_stress[1:9])
#'
#'#### Perform the DIF analysis
#'out <- ANCOVA.dif(work_stress[1:9], work_stress$gender, Trait)
#'out
#' @export
ANCOVA.dif <- function(x, Group, Trait){
  options(scipen = 1)
  data <- data.frame(Trait, Group, x)
  len2 <- length(data)    ### No of variables in data frame

  namesCol<- c("Item","Trait_p","Group_p","TxG_p",  "NA")

  AOV_Frame          <- as.data.frame(matrix(, ncol = 5, nrow = 0))  ## Create empty data frame
  names(AOV_Frame)   <- namesCol       ### Give names to the columns
  len                <- length(x)            ### The number of items

  namesCol2          <- c("Item","Trait_eta.sq","Group_eta.sq","TxG_eta.sq",  "NA")

  Etasq_Frame        <- as.data.frame(matrix(, ncol = 5, nrow = 0))  ## Create empty data frame
  names(Etasq_Frame) <- namesCol2       ### Give names to the columns


  len21 <- length(data)                 ### Number of variables in the original data
  options(digits = 4)

  for (p in 1:len) {

    counter <- 2 + p                 ### First item is in column 3

    ### Obtain the p values
    items <- summary(aov(data[, counter] ~ data[, 1]*data[, 2]))[[1]][["Pr(>F)"]]

    ### Obtain eta_squared
    library(effectsize)
    etasq <- eta_squared(aov(data[, counter] ~ data[, 1]*data[, 2]), partial = FALSE)[, 2]

    ### Place the p values in the AOV_Frame
    AOV_Frame[p, 1] <- names(data)[counter]
    AOV_Frame[p, 2] <- items[[1]]
    AOV_Frame[p, 3] <- items[[2]]
    AOV_Frame[p, 4] <- items[[3]]
    AOV_Frame[p, 5] <- items[[4]]

    ### Place the eta squared values in the Etasq_Frame
    Etasq_Frame[p, 1] <- names(data)[counter]
    Etasq_Frame[p, 2] <- etasq[1]
    Etasq_Frame[p, 3] <- etasq[2]
    Etasq_Frame[p, 4] <- etasq[3]
    Etasq_Frame[p, 5] <- etasq[4]

    mostUDIF  <- Etasq_Frame[which.max(Etasq_Frame$Group_eta.sq), 1:4]
    mostNUDIF <- Etasq_Frame[which.max(Etasq_Frame$TxG_eta.sq), 1:4]
  }

Results <- cbind(AOV_Frame[, c(1:4)],
                 Etasq_Frame[, c(2:4)])

Results <- data.frame(Results[1],
                      round(Results[2:7], 6))

Results <- list("Results"              = Results,
                "Most.uniform.dif"     = mostUDIF,
                "Most.non.uniform.dif" = mostNUDIF)

return(Results)
}
