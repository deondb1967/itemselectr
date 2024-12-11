# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Item selection based on McDonald's item information statistic
#' 
#' An item selection procedure based on item information statistics.
#' 
#' @param x A data frame containing items
#' @param nitems The number of items to retain in the scale
#' @return The function sequentially removes the item with the lowest information statistic from a pool of items. The output is a list containing (a) a data frame that shows at each step of the item selection process the number of items that had been removed, the name of the item that is removed, and the reliability of the scale; (b) the names of the items that were retained; (c) coefficient omega of the final scale, and (d) coefficient alpha of the final scale.
#' @examples
#' library(lordif)
#' data(Anxiety)
#' info.select(Anxiety[4:32], 5)
#' @export 
    info.select <- function(x, nitems) {
      h2   <- psych::fa(x, 1, cor = "cov")$communality
      u2   <- psych::fa(x, 1, cor = "cov")$uniqueness
      info <- sort(h2/u2)

#      StepOmega         <- as.data.frame(c(0, "None", MBESS::ci.reliability(x, type = "hierarchical")[1]))
      
      
      ## Data frame that tracks omega as you remove items iteratively
      StepOmega         <- as.data.frame(c(0, "None", data.frame(psych::omega(x, nfactors = 1)$omega.tot)))
      names(StepOmega)  <- c('Items.out','Item.removed', 'Omega')

      ## Specify number of items you want
      items.wanted <- nitems

      for (i in 1:(ncol(x) - items.wanted)) {
        # Identify item with lowest information
        low.info        <- names(info[i])
        item.out <- names(x) %in% c(low.info)

        ### Remove item with the lowest information from data frame
        x <- x[!item.out]

        # Calculate omega with MBESS package
        #rel             <- MBESS::ci.reliability(x, type = "hierarchical")$est
        
        # Calculate omega with psych package
        rel             <- psych::omega(x, nfactors = 1)$omega.tot

        # Store the step, name of item removed and omega in a data frame
        LoopOut         <- data.frame(i, low.info,rel)
        names(LoopOut ) <- c('Items.out', 'Item.removed','Omega')

        StepOmega       <-  rbind(StepOmega,LoopOut)
      }
      results   <- list(Steps          = StepOmega,
                        Items.in.scale = names(x),
                        Omega          = suppressMessages(psych::omega(x, nfactors = 1)$omega.tot),
                        Alpha          = psych::alpha(x)$total[1])
      print(results)
    }
