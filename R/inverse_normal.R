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
	v <- v1 <- data.frame(index=1:length(v), v=v)
	v1 <- stats::na.omit(v1)
	v <- v[!v$index %in% v1$index, ]
	
	mrank <- rank(v1$v, na.last=TRUE)
	v1$v <- stats::qnorm(mrank/max(mrank+1))
	
	v <- rbind(v1, v)
	v <- v[order(v$index), ]
	t(v)[2,]
}