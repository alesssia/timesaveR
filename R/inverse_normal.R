inverse.normal <- function(v)
# Performs the inverse normal transformation
#
# Args:
#	v	: vector of values
# Output:
# 	a vector of transformed vectors
#
{
	#I need to do some tricks, otherwise rank+qnorm will
	#impute my missing data
	v <- v1 <- as.data.frame(cbind(1:length(v), v))
	colnames(v) <- colnames(v1) <- c("index", "v")
	v1 <- na.omit(v1)
	
	mrank <- rank(v1$v, na.last=T)
	v1$v <- qnorm(mrank/max(mrank+1))
	
	v <- merge(v, v1, by="index", all=T)
	v$v.y
}