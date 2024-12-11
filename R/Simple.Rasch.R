# Generated from create-itemselectr-spare.Rmd: do not edit by hand

#' Rasch analysis of dichotomous or polytomous items
#' 
#' Basic Rasch analysis of dichotomous or polytomous items. This function is a wrapper for the tam.mml() function of the TAM package. The purpose of the function is to simplify the Rasch analysis process and to produce the output that users most typically want.
#' 
#' @param raschdata A data frame containing items where the lowest response category is scored as 0
#' @param irtmodel The irt model. "PCM", "PCM2" or "RSM"
#' @return The output is a list containing the results of the analysis.
#' @examples
#' library(TAM)
#' library(lordif)
#' data(Anxiety)
#' Anxiety <- Anxiety - 1 
#' 
#' ## Fit the rating scale model
#' myoutput <- Simple.Rasch(raschdata = Anxiety[4:32], irtmodel = "RSM")
#' ls(myoutput)
#' myoutput$item.fit
#' 
#' #' ## Fit the partial credit model
#' myoutput <- Simple.Rasch(raschdata = Anxiety[4:32], irtmodel = "PCM2")
#' ls(myoutput)
#' myoutput$item.fit
#' 
#' @export 
Simple.Rasch <- function(raschdata, irtmodel) {
  
  ### Save the graphics default settings
  old.par <- par()
  
  library(psych)
  
  library(TAM)
  ### Estimate and inspect the item parameters
  myTAM <- TAM::tam.mml(raschdata, irtmodel = irtmodel, verbose = FALSE)
  summary(myTAM)
  
  ### Inspect the item difficulty parameters and standard errors
  (deltas <- myTAM$xsi)
  deltas <- deltas[1: length(raschdata),] 
  
  ### Inspect the item difficulty and tau parameters (Rasch-Andrich thresholds)
  myTAM$item_irt
  
  ### Now sort the items according to their difficulty
  deltas[order(deltas$xsi), ]
  
  ### Plot the expected and observed item characteristic curves
  plot(myTAM, type = "expected", ngroups = 6, low = -3, high = 5)
  
  ## Plot the option characteristic curves
  plot(myTAM, type = "items", ngroups = 6, low = -3, high = 5)
  
  ### Inspect the item infit and outfit statistics
  TAM::msq.itemfit(myTAM)$itemfit
  
  ### Inspect Yen's Q3 statistics for pairs of items
  TAM::tam.modelfit(myTAM)$stat.itempair
  
  ### Inspect the item root mean square discrepancies (RMSD)
  (item.RMSD <- CDM::IRT.itemfit(myTAM)$RMSD_bc)
  
  ### Find and store the standardised residuals
  myTAMresid <- TAM::IRT.residuals(myTAM)$stand_residuals
  myTAMexpct <- TAM::IRT.residuals(myTAM)$X_exp
  
  ### Create a scree-plot of the standardized residuals
  psych::scree(myTAMresid, factors = FALSE)
  
  ### Perform an unrotated principal components analysis of the residuals
  (pcaresid <- psych::pca(myTAMresid, nfactors = ncol(myTAMresid), rotate = "none"))
  #pca(myTAMresid, nfactors = nitems, rotate = "varimax")
  
  ### Plot the test information curve
  info.myTAM <- TAM::IRT.informationCurves(myTAM)
  plot(info.myTAM, xlab = "Theta", ylab = "Test information", main = "Test Information Curve",
       xlim = c(-3, 5))
  
  ### Plot the standard error curve
  plot(info.myTAM, curve_type = "se", xlab = "Theta", ylab = "Standard error", main = "Test Standard Error Curve",
       xlim = c(-3, 5),
       ylim = c(0, 3.5))
  
  
  ### Obtain the estimated person measures (EAP)
  pmod1 <- CDM::IRT.factor.scores(myTAM, type = "EAP")
  
  ### Histogram and boxplot of the person measures 
  hist(pmod1$EAP, 
       xlab = "Person measures in logits", 
       main = NULL,
       xlim = c(-5, 5))
  boxplot(pmod1$EAP, ylab = "Person measures in logits")
  
  ### Plot a construct alley using outfit
  plot(TAM::msq.itemfit(myTAM)$itemfit$Outfit, 
       deltas$xsi, xlim = c(0.3, 1.7), 
       type = "n",
       xlab = "Outfit mean square", 
       ylab = "Item location in logits")
  text(TAM::msq.itemfit(myTAM)$itemfit$Outfit, 
       deltas$xsi, 
       labels = colnames(raschdata), 
       cex    = 0.9, 
       font   =2)
  abline(v = 1)
  abline(v = 1.30, col = "blue")
  abline(v = .70, col = "blue")
  
  ### Plot a construct alley using infit
  plot(TAM::msq.itemfit(myTAM)$itemfit$Infit, 
       deltas$xsi, xlim = c(0.3, 1.7), 
       type = "n",
       xlab = "Infit mean square", 
       ylab = "Item location in logits")
  text(TAM::msq.itemfit(myTAM)$itemfit$Infit, 
       deltas$xsi, 
       labels=colnames(raschdata), 
       cex=0.9, 
       font=2)
  abline(v = 1)
  abline(v = 1.30, col = "blue")
  abline(v = .70, col = "blue")

  ### Restore the default graphic parameters
  #par(old.par)
  
  ### Plot a Wright map
  #IRT.WrightMap(myTAM, show.thr.sym = FALSE)
  
  ### Restore the default graphic parameters
  #par(old.par)
  
  
  ### Descriptive statistics of the person measures
  psych::describe(pmod1$EAP)
  
  ### Reliability of the person measures
  TAM::EAPrel(pmod1$EAP, pmod1$SD.EAP)
  
  ### Person fit
  my.personfit <- TAM::tam.personfit(myTAM)
  my.person.outfit <- my.personfit[order(my.personfit$outfitPerson, 
                                         decreasing = TRUE), ]
  my.person.infit  <- my.personfit[order(my.personfit$infitPerson,
                                         decreasing = TRUE), ]
  mylist <- list(deltas = deltas,
                 deltas.ordered = deltas[order(deltas$xsi), ],
                 deltas.taus = myTAM$item_irt,
                 model.fit = TAM::tam.modelfit(myTAM)$statlist,
                 item.fit = TAM::msq.itemfit(myTAM)$itemfit,
                 item.RMSD = item.RMSD, 
                 YenQ3 = TAM::tam.modelfit(myTAM)$stat.itempair,    
                 std.residuals = myTAMresid,
                 item.expected = myTAMexpct,
                 pca.residuals = pcaresid, 
                 thetas = pmod1, 
                 describe.theta = psych::describe(pmod1$EAP),
                 WLE.reliability = TAM::WLErel(pmod1$EAP, pmod1$SD.EAP),
                 Cronbach.reliability = psych::alpha(raschdata),
                 person.outfit = head(my.person.outfit, 100),
                 person.infit = head(my.person.infit, 100)
  )
}

