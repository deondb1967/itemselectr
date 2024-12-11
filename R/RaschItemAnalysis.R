# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Rasch analysis of polytomous items with the partial credit model
#' 
#' Basic Rasch analysis of polytomous items using the partial credit model. This function is a wrapper for the PCM() function of the eRm package. The purpose of the function is to simplify the Rasch analysis process and to produce the output that users most typically want.
#' 
#' @param x A data frame containing items where the lowest response category is scored as 0
#' @return The output is a list containing the results of the analysis.
#' @examples
#' library(lordif)
#' data(Anxiety)
#' Anxiety <- Anxiety[4:32] - 1 
#' 
#' ## Fit the partial credit model
#' myoutput <- RaschItemAnalysis(x = Anxiety)
#' ls(myoutput)
#' myoutput$Infit.outfit 
#' @export 
RaschItemAnalysis <- function(x) {
  pcm.mod <- eRm::PCM(x)
  
  ### Martin Loef test of invariance of person parameters across item subgroups
  MLoef.1 <- eRm::MLoef(pcm.mod)
  
  ### Estimation of the person parameters
  p.pcm.mod <- eRm::person.parameter((pcm.mod))
  
  ### Proportion of persons with misfit
  prop.misfit.persons <- eRm::PersonMisfit(p.pcm.mod)
  
  ### Distribution of the person parameters
  theta.pcm.mod <- p.pcm.mod$thetapar$NAgroup1
  hist(theta.pcm.mod)
  psych::describe(theta.pcm.mod)
  
  ### Item and test information plots
  eRm::plotINFO(pcm.mod, legpos = FALSE)
  
  ### Person separation reliability
  reliab <- summary(eRm::SepRel(p.pcm.mod))
  
  ### Principal components analysis of the standardized residuals
  pfit    <- eRm::personfit(p.pcm.mod)
  pca.out <- psych::pca(pfit$st.res, nfactors = ncol(x), rotate = "none")
  psych::scree(pfit$st.res, factors = FALSE, main = "Scree plot of standardized Rasch residuals")
  
  ### Item location and threshold parameters
  thresholds.out <- eRm::thresholds(pcm.mod)
  
  ### Wright Person-Item map
  eRm::plotPImap(pcm.mod)
  
  ### Infit and outfit statistics
  fit.stats <- iarm::out_infit(pcm.mod)
  
  ### Compare observed and expected mean item scores
  obs.exp.means <- iarm::item_obsexp(pcm.mod)
  
  ### Compare observed and expected gamma coefficients of items and restscore
  obs.exp.gamma <- iarm::item_restscore(pcm.mod)
  
  ### Item targetting
  iarm::item_target(pcm.mod)
  
  ### Examine local independence
  loc.indep <- iarm::partgam_LD(x)
  
  ### Properties of the test
  test.prop <- iarm::test_prop(pcm.mod)
  
  out <- list(Martin.Loef.test = MLoef.1,
              Proportion.misfitting.persons = eRm::PersonMisfit(p.pcm.mod),
              Distribution.theta = psych::describe(theta.pcm.mod),
              Person.Separation.Reliability = eRm::SepRel(p.pcm.mod),
              PCA.standarized.residuals = pca.out,
              Item.locations.thresholds = thresholds.out,
              Infit.outfit = fit.stats,
              Observed.expected.item.means = obs.exp.means,
              Observed.expected.gammas = obs.exp.gamma,
              Local.independence = loc.indep,
              Test.properties = test.prop)
}
