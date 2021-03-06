inverse.normal <- function(v)
#' Performs the inverse normal transformation
#'
#' Performs the inverse normal transformation
#'
#' @author Alessia Visconti
#' @param v	vector of values
#' @return a vector of transformed vectors
#' @examples
#' x <- rexp(100)
#' inverse.normal(x)
#' @export
{
	#I need to do some tricks, otherwise rank+qnorm will
	#impute my missing data
	v <- v1 <- as.data.frame(cbind(1:length(v), v))
	colnames(v) <- colnames(v1) <- c("index", "v")
	v1 <- stats::na.omit(v1)
	
	mrank <- rank(v1$v, na.last=TRUE)
	v1$v <- stats::qnorm(mrank/max(mrank+1))
	
	v <- merge(v, v1, by="index", all=TRUE)
	v$v.y
}