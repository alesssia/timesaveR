estimate.heritability <- function(myformula, data, DZ="DZ", zyg="ZYGOSITY", fid="FID", checks.CI=TRUE)
#' Estimates ACE, AE, CE, and E heritability models
#'
#' Estimate the contribution of additive genetic (A), shared (C) and individual-specific 
#' environment (E) effects on traits variations, using a classical twin design study. 
#' It fits ACE, and the most parsimonious AE, CE e E model, which are then compared using 
#' the Akaike’s information criterion (AIC). The best (lowest AIC) model is then selected.
#' If checks.CI is set, when selecting the best model, the functions also checks that the
#' confidence intervals (CI) do not include the zero -- if they do, the next best nodel is
#' selected and a note is returned.
#'
#' @author Alessia Visconti
#' @param myformula the model to be fit (trait ~ 1 + covariate)
#' @param data input data frame
#' @param DZ how dyzigotic twins are codified
#' @param zyg name of the column describing zygosities
#' @param fid name of the column describing family ID
#' @param checks.CI whether to check if the best model CI includes the zero, and the next model should be chosen instead
#' @return a list with the following R objects:
#' , models, a list of the heritability summary stats for each tested model, that is:
#' , models$ACE, the results of the ACE model
#' , models$AE, the results of the AE model
#' , models$CE, the results of the CE model
#' , models$E, the results of the E model
#' , aics, AICs of all fitted models
#' , best, label for best model (ACE, AE, CE, E)
#' , estimate.best, the summary stats and CI for the best model
#' , note, whether a next-best model has been used (empty if check.CI is FALSE)
#' @export
{
	#Evaluates all heritability models
	models <- list()
	models$ACE <- mets::twinlm(stats::formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="ace")
	models$AE <- mets::twinlm(stats::formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="ae")
	models$CE <- mets::twinlm(stats::formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="ce")
	models$E <- mets::twinlm(stats::formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="e")
	
	#Evaluates AIC
	aics <- c(stats::AIC(models$ACE), stats::AIC(models$AE), stats::AIC(models$CE), stats::AIC(models$E))
	names(aics) <- c("ACE", "AE", "CE", "E")
	
	#Sorts best to worse
	aics <- aics[order(aics)]
	
	#The lowest is the best fit (no note to be returned if the CIs are fine, or checks.CI is FALSE)
	which.best <- 1
	note <- ""
	
	#Selects model corresponding to the best AIC, and extract estimates with CI
	best <- names(aics)[which.best]
	estimate.best <- round(as.data.frame(summary(models[[which(names(models) == best)]])$acde)*100, 1)
	
	if (checks.CI)
	{
		# For E it's normal to include the zero
		while (sum(estimate.best$`2.5%` <= 0) != 0 & best != "E")
		{
			which.best <- which.best + 1
			#FIXME: could this also throw a warning?
			note <- "Previous models were discarded because their CI included the zero"
	
			best <- names(aics)[which.best]
			estimate.best <- round(as.data.frame(summary(models[[which(names(models) == best)]])$acde)*100, 1)
		}
	}
	
	list(models=models, aics=aics, best=best, estimate.best=estimate.best, note=note)
}
