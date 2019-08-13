remove.outliers <- function(v, n.sd=3)
#' Sets outliers to NA
#' 
#' Sets to NA the values greater/smaller than n.sd standard deviation from
#' the values' mean. 
#'
#' @author Alessia Visconti
#' @param v vector of values
#' @param n.sd number of standard deviation (default: 3)
#' @return a vector having outliers set to NA
#' @examples
#' x <- rexp(100)
#' remove.outliers(x)
#' @export
{
	avg <- mean(v, na.rm=TRUE)
	stdev <- sd(v, na.rm=TRUE)
	v[v <= (avg - n.sd*stdev) | v >= (avg + n.sd*stdev)] <- NA
	v
}

