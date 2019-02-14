to.chisq <- function(values, type="pvalue")
# Transform statistical distribition to chi-squared values
#
# Args:
#   values   : the statistic distribution
#   type	 : statistic distribution (allowed: zscore, chisq, pvalue -- default: pvalue)
# Output:
#   the chi-squared values
#
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
		chisq <- values^2
	}
	else if (type == "chisq")
	{
		# For chi-squared values, keep as is
		chisq <- values
	}
	else if (type == "pvalue")
	{
		# For p-values, calculate chi-squared statistic
		chisq <- qchisq(1-values, 1)
	}
	else
	{
		stop("Allowed value types are: <\"zscore\", \"chisq\", \"pvalue\"> -- default \"pvalue\" ")
	}
	
	chisq
}
