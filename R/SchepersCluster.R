#' Item selection based on inter-item covariances
#'
#' A bottom-up item selection procedure based on inter-item covariances.
#'
#' @param x Data frame containing items that are all scored in the same direction
#' @param nitems The number of items to select (this value should be no lower than four)
#' @return A list containing (a) the two items that constitute the core, (b) a data frame containing scale statistics, and (c) the names of the items that are included in the scale
#' @examples
#' # Build a five-item scale using hierarchical clustering approach
#' SchepersCluster(work_stress[1:9], 5)
#' @export
SchepersCluster <- function(x, nitems) {
  library(psych)
  library(MBESS)

  items.in.scale <- nitems
  items.wanted   <- items.in.scale - 3

  ### Create covariance matrix of the items
  mycov <- abs(cov(x, use = "complete.obs"))

  ## Step 1
  ## Find the highest covariance in the matrix
  max(abs(mycov[upper.tri(mycov)]))

  ## Find the names of the variables involved in the highest covariance
  core1 <- which(mycov == max(abs(mycov[upper.tri(mycov)])), arr.ind = T)
  core1.names <- row.names(core1)
  core1.names

  ## Find the total score of two items with highest covariance (Total1)
  total1 <- rowSums(x[, row.names(core1)])

  # Items in scale
  items <- colnames(x) %in% core1.names
  #items


  ##Step 2
  ## Find the covariances of the remaining items with total1
  cov(total1, x[, !items])

  ## Find the highest item-total covariance
  max(abs(cov(total1, x[, !items])))

  ## Find the name of the variable with the highest item-total covariance
  v_name = colnames(x[, !items])
  add.item <- v_name[which.max(cov(total1, x[, !items]))]

  ## Find new total score (Total2)
  total2 <- total1 + x[, add.item]

  core1.names <- c(core1.names, add.item)
  omega  <- ci.reliability(x[core1.names])$est
  myfa   <- fa(x[core1.names])
  udim   <- unidim(x[core1.names])
  myrel1 <- data.frame(omega = round(omega, 3),
                       alpha = round(udim$uni[4], 3),
                       uni = round(udim$uni[1], 3),
                       tau = round(udim$uni[2], 3),
                       con = round(udim$uni[3], 3),
                       av.r = round(udim$uni[5], 3),
                       ECV = round(udim$uni[8], 3),
                       TLI = round(myfa$TLI, 3),
                       RMS = round(myfa$rms, 3),
                       CFI = round(myfa$CFI, 3),
                       F1.F2 = round(udim$uni[9], 2),
                       added.item = core1.names[length(core1.names)],
                       n.items = length(core1.names))
  rel <- myrel1
  rel

  # Items in scale
  items <- colnames(x) %in% core1.names
  core1.names


  ### Create a counter
  counter <- numeric(0)


  ### Start the loop
  for(i in 1:items.wanted) {

    counter[i]

  ## Step3
  ## Find the covariances of the remaining items with total2
  cov(total2, x[, !items])

  ## Find the highest item-total covariance
  max(abs(cov(total2, x[, !items])))

  ## Find the name of the variable with the highest item-total covariance
  v_name = colnames(x[, !items])
  add.item <- v_name[which.max(cov(total2, x[, !items]))]

  ## Find the names of the items in the scale
  core1.names <- c(core1.names, add.item)

  ## Find new total score (total2)
  total2 <- total2 + x[, add.item]
  omega  <- MBESS::ci.reliability(x[core1.names])$est
  myfa   <- fa(x[core1.names])
  udim   <- unidim(x[core1.names])
  myrel2 <- data.frame(omega      = round(omega, 3),
                       alpha      = round(udim$uni[4], 3),
                       uni        = round(udim$uni[1], 3),
                       tau        = round(udim$uni[2], 3),
                       con        = round(udim$uni[3], 3),
                       av.r       = round(udim$uni[5], 3),
                       ECV        = round(udim$uni[8], 3),
                       TLI        = round(myfa$TLI, 3),
                       RMS        = round(myfa$rms, 3),
                       CFI        = round(myfa$CFI, 3),
                       F1.F2      = round(udim$uni[9], 2),
                       added.item = core1.names[length(core1.names)],
                       n.items = length(core1.names))
  rel    <- rbind(rel, myrel2)
  row.names(rel) <- NULL
  #print(round(rel, 3))

  # Items in scale
  items <- colnames(x) %in% core1.names
  #core1.names

  #out <- list(rel,
   #           Items.in.scale = core1.names)

  #print(out)

  #plot(rel$omega)

  }
  out <- list(core             = core1.names[1:2],
              scale.statistics = rel,
              Items.in.scale   = core1.names)

  plot(rel$omega)
  print(out)

}

