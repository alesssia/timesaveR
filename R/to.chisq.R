to.chisq <- function(values, type=c("zscore", "chisq", "pvalue"))
#' Transform statistical distribition to chi-squared values
#' 
#' Given a vector of numerical values and which type of statistical
#' distribition thet are (i.e., chisq, pvalue, zscore) it coverts 
#' them in chi-squared.
#'
#' @author Alessia Visconti
#' @param values a vector of numerical values
#' @param type which type of statistic distribution
#' @return chi-squared values
#' @examples
#' pvalues <- abs(rnorm(100, mean=0, sd=0.1))
#' to.chisq(pvalues, "pvalue")
#' @export
{
	# Values should be numeric
	if (class(values) != "numeric")
	{
		stop("Numeric values required.")
	}

	# The chi-squared statistic depends on the type of data at hand
	# Here we manage zscores, chisqs and pvalues.
	if (type == "zscore")
	{
		# For z-scores, just square them
		return (values^2)
	}
	else if (type == "chisq")
	{
		# For chi-squared values, keep as is
		return (values)
	}
	else if (type == "pvalue")
	{
		# For p-values, calculate chi-squared statistic
		return (stats::qchisq(1-values, 1))
	}
	else
	{
		stop("Allowed value types are: <\"zscore\", \"chisq\", \"pvalue\"> -- default \"pvalue\" ")
	}

	NA
}
