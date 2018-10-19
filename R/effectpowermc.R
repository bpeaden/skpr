#'@title Fit Anova for Effect Power Calculation in Monte Carlo
#'
#'@description Calculates the p-values for the effect power calculation in Monte Carlo
#'
#'@param fit Fit from regression
#'@param type Default `III`
#'@param test Default `Pr(>Chisq)`.
#'@param ... Additional arguments to pass to car::Anova
#'@return p-values
#'@keywords internal
effectpowermc = function(fit, type="III", test = "Pr(>Chisq)", splitplot=FALSE, ...) {
  if(splitplot) {
    if(class(fit) == "glmerMod") {
      anovafit = car::Anova(fit, type = type, test.statistic="Chisq" )
      effectnames = rownames(anovafit)
      effect_pvals = as.vector(as.matrix(anovafit[test]))
    } else {
      if(test != "Pr(>F)") {
        warning("Effect power for blocked/split-plot designs only supports F-tests. Ignoring test argument.")
      }
      test = "Pr(>F)"
      anovafit = suppressMessages(anova(fit,type=type, ...))
      effectnames = rownames(anovafit)
      effect_pvals = as.vector(as.matrix(anovafit[test]))
    }
  } else {
    anovafit = car::Anova(fit, type = type, ... )
    effectnames = rownames(anovafit)
    effect_pvals = as.vector(as.matrix(anovafit[test]))
  }
  if ("Residuals" %in% effectnames) {
    effect_pvals = effect_pvals[-length(effect_pvals)]
    names(effect_pvals) = effectnames[-length(effectnames)]
  } else {
    names(effect_pvals) = effectnames
  }
  effect_pvals
}
