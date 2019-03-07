estimate.heritability <- function(myformula, data, DZ="DZ", zyg="ZYGOSITY", fid="FID")
# Estimate the contribution of additive genetic (A), shared (C) and individual-specific 
# environment (E) effects on traits variations, using a classical twin design study. 
# It fits ACE, and the most parsimonious AE, CE e E model, which are then compared using 
# the Akaike’s information criterion (AIC).
#
# Args:
#	myformula: the model to be fit (trait ~ 1 + covariate)
#	data	 : input data frame
#	DZ		 : how dyzigotic twins are codified
#	zyg		 : name of the column describing zygosities
#	fid		 : name of the column describing family ID
# Output:
# 	a list including
#	- ace, the results of the ACE model
#	- ae, the results of the AE model
#	- ce, the results of the CE model
#	- e, the results of the E model
#	- aics, AICs of all fitted models
#	- best.m, label for best model (ACE, AE, CE, E)
#	- best model, the results of the best model
#
{
	#Evaluates all heritability models
	models <- list()
	models$ACE <- mets::twinlm(as.formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="ace")
	models$AE <- mets::twinlm(as.formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="ae")
	models$CE <- mets::twinlm(as.formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="ce")
	models$E <- mets::twinlm(as.formula(myformula), data=data, DZ=DZ, zyg=zyg, id=fid, type="e")
	
	#Evaluates AIC
	aics <- c(AIC(models$ACE), AIC(models$AE), AIC(models$CE), AIC(models$E))
	names(aics) <- c("ACE", "AE", "CE", "E")
	
	#Selects model corresponding to the best AIC, and extract estimates with CI
	best <- names(aics)[aics == min(aics)]
	estimate.best <- round(as.data.frame(summary(models[[which(names(models) == best)]])$acde)*100, 1)
	
	list(models=models, aics=aics, best=best, estimate.best=estimate.best)
}