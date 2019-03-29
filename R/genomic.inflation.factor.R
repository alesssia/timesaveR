genomic.inflation.factor <- function(values, type="pvalue")
# Evalutes the genomic inflation factor (lambda) for genome-wide association studies. 
# In details, lambda is the observed median value of the chi-squared statistic for the 
# null markers divided by the expected median  value of the chi-squared statistic 
# (approximately 0.456 for 1 degree of freedom tests).
#
# Args:
#   values   : the statistic distribution
#   type	 : statistic distribution (allowed: zscore, chisq, pvalue -- default: pvalue)
# Output:
#   the “inflation factor” lambda
#
{
	chisq <- to.chisq(values, type)
	median(chisq)/qchisq(0.5,1)
}

