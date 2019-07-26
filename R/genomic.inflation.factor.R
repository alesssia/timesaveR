genomic.inflation.factor <- function(values, type="pvalue")
#' Evalutes the genomic inflation factor (lambda)
#'
#' Evalutes the genomic inflation factor (lambda) for genome-wide association studies. 
#' In details, lambda is the observed median value of the chi-squared statistic for the 
#' null markers divided by the expected median  value of the chi-squared statistic 
#' (approximately 0.456 for 1 degree of freedom tests).
#'
#' @author Alessia Visconti
#' @param values the statistic distribution
#' @param type statistic distribution (allowed: zscore, chisq, pvalue -- default: pvalue)
#' @return the "inflation factor" lambda
#' @examples
#' pvalues <- abs(rnorm(1000, mean=0.001, sd=0.1))
#' genomic.inflation.factor(pvalues, "pvalue")
{
	chisq <- to.chisq(values, type)
	median(chisq)/qchisq(0.5,1)
}

