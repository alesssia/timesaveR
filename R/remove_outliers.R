remove.outliers <- function(v, n.sd=3)
# Sets to NA the values greater/smaller than n.sd standard deviation from
# the values' mean. 
#
# Args:
#	v	: vector of values
#	n.sd: number of standard deviation (default: 3)
# Output:
# 	a vector having outliers set to NA
{
	avg <- mean(v, na.rm=T)
	stdev <- sd(v, na.rm=T)
	v[v <= (avg - n.sd*stdev) | v >= (avg + n.sd*stdev)] <- NA
	v
}

