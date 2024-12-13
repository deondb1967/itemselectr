#' Identify influential cases and examine their impact
#'
#' Calculates generalized Cook's Distance statistics. Removes a specified number of cases with largest gCD values. Compares factor loadings, fit statistics and reliabilities of full data set with the filtered data set.
#'
#' @param x Data frame containing items
#' @param ncases The number of cases to treat as influential. This value can be set to 1 and then adjusted in subsequent analyses.
#' @return A list containing (a) the factor loadings, (b) fit statistics of a single factor solution, and (c) the Cronbach alpha coefficients of the full and filtered data sets.
#' @examples
#'
#' influential.cases(work_stress[1:9], 10)
#' @export
influential.cases <- function(x, ncases = 1) {
a <- toString(names(x))
b <- "Factor=~"
c <- gsub(",","+", a)
model <- paste(b,c)

gd <- faoutlier::gCD(x, model)
plot(gd$gCD)
cases <- tail(sort(gd$gCD), ncases)
myoutliers <- names(cases)
myoutliers


# Exclude rows in myoutliers
x_filtered <- x[!rownames(x) %in% myoutliers, ]


all <- psych::fa(x)
filtered <- psych::fa(x_filtered)

loadings <- round(cbind(all$loadings, filtered$loadings), 2)
colnames(loadings) <- c("All", "Filtered")

srmr <- c(all$rms, filtered$rms)
names(srmr) <- c("All", "Filtered")

RMSEA <- c(all$RMSEA[1], filtered$RMSEA[1])
names(RMSEA) <- c("All", "Filtered")

TLI <- c(all$TLI, filtered$TLI)
names(TLI) <- c("All", "Filtered")

CFI <- c(all$CFI, filtered$CFI)
names(CFI) <- c("All", "Filtered")

fit <- round(rbind(srmr, RMSEA, TLI, CFI), 3)

alpha1 <- psych::alpha(x, check.keys = TRUE)$total[1]
alpha2 <- psych::alpha(x_filtered, check.keys = TRUE)$total[1]

alphas <- c(alpha1$raw_alpha, alpha2$raw_alpha)
names(alphas) <- c("All", "Filtered")

myoutput <- list(influential.cases = cases,
                 loadings = loadings,
                 fit = fit,
                 Cronbach.alphas = alphas)
print(myoutput)
}
