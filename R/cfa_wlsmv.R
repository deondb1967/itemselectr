# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Confirmatory factor analysis of ordinal items with WLSMV estimation
#' 
#' Fits a one-factor CFA model with uncorrelated errors to the polychoric correlations of the items with WLSMV estimation. The function is a wrapper for the cfa() function of the lavaan package.
#' 
#' @param x A data frame containing the items to be analysed
#' @return The output is a list containing the results of the analysis.
#' @examples
#' 
#' library(psych)
#' data(bfi)
#' 
#' cfa_wlsmv(x)
#' 
#' @export
cfa_wlsmv <- function(x) {
  a <- toString(names(x))
  b <- "Factor=~"
  c <- gsub(",","+", a)
  model <- paste(b,c)
  
  fit.model  <- lavaan::cfa(model, data = x, ordered = TRUE, 
                            estimator = "WLSMV", std.lv = TRUE)
  pars       <- lavaan::parameterestimates(fit.model)
  fitindices <- lavaan::fitMeasures(fit.model, (c("pvalue", "npar", "chisq", "df", 
                                          "tli.robust", "cfi.robust", 
                                          "rmsea.robust", "rmsea.ci.lower.robust",
                                          "rmsea.ci.upper.robust", "srmr_bentler")))
  modind   <- lavaan::modificationindices(fit.model)
  modresid <- lavaan::resid(fit.model, type = "cor")$cov
  
  rel <- semTools::reliability(fit.model)
  
  results.list <- list(model = model,
                    factor.loadings = pars[pars$op == "=~", ],
                    unique.variances = pars[pars$op == "~~", ],
                    fit.indices = fitindices,
                    modification.indices = modind,
                    standardized.residuals = modresid,
                    reliability = rel
      )
  print(results.list)
}

