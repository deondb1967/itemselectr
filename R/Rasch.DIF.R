#' Performs a differential item functioning analysis of polytomous items using ANCOVA
#'
#' Differential item functioning analysis of polytomous items via ANCOVA
#'
#' @param std.res A data frame containing the standardized residuals of a Rasch analysis
#' @param group A factor indicating group membership
#' @param thetas A vector containing measures of the trait. This may be the total scores across all the items, the mean item scores, factor scores, or IRT person measures.
#' @examples
#'# Lowest response category must be scored as 0
#'work_stress0 <- work_stress[1:9] - 1
#'d <- Simple.Rasch(work_stress0, "PCM2")
#'
#'# Store the standardized residuals as a data frame
#'d$std.residuals <- data.frame(d$std.residuals)
#'
#'# Perform the dif analysis
#'out <- Rasch.DIF(d$std.residuals, work_stress$gender, d$thetas[,1])
#'out
#' @export
Rasch.DIF  <- function(std.res, group, thetas) {
  nitems   <- ncol(std.res)

  myresids <- data.frame(std.res, group, thetas)
  myresids <- data.frame(myresids[1:nitems],
                         setNames(myresids[, (nitems + 1):(nitems+ 2)], c("Group","Theta")))
  library(foreach)
  zzz <- foreach(i=1:nitems) %do%
         summary(lm(myresids[,i] ~ myresids$Theta*myresids$Group))$coefficients[2:4, ]#[[i,4]]

  names(zzz) <-colnames(myresids[,1:nitems])

  ### Create empty data frame with one value
  DFZ         <- data.frame(
    Iteration   = numeric(1),
    Item        = character(1),
    Trait       = numeric(1),
    Group       = numeric(1),
    TraitxGroup = numeric(1)
  )

  ### Loop that creates the table with p-values
  for (j in 1:length(zzz) ) {
    p.value           <-  as.data.frame(zzz[j])[,4]  #$Pr...t.
    rowName           <-  as.character(names(zzz)[j])
    p.table           <-  data.frame(j, rowName, p.value[1], p.value[2], p.value[3])
    colnames(p.table) <-  c("Iteration", "Item", "Trait", "Group", "TraitxGroup")
    DFZ               <-  rbind(DFZ,  p.table)
  }

  DFZ <- DFZ[-1, ]

  DFZ <- data.frame(DFZ[1:2], round(DFZ[3:5], 5))

  results <- list(Entry.order           = DFZ,
                  Trait.fit.order       = DFZ[order(DFZ$Trait, decreasing = TRUE), ],
                  Uniform.DIF.order     = DFZ[order(DFZ$Group, decreasing = TRUE), ],
                  Non_uniform.DIF.order = DFZ[order(DFZ$TraitxGroup, decreasing = TRUE), ])

  return(results)
}
