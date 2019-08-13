estimate.meff <- function(data, method = c("Cheverud", "Li"))
#' Estimates the effective number of tests
#'
#' Estimates the effective number of tests in correlated datasets using two different 
#' approaches (Li's is to be preferred)
#' Cheverud JM (2001). A simple correction for multiple comparisons in interval mapping
#' genome scans. Heredity 87
#'
#' @author Jonas Zierer
#' @param data data matrix
#' @param method which methods should be used
#' @return effective number of tests
#' @examples
#' data <- matrix(data = rnorm(200), nrow = 10, ncol = 20)
#' estimate.meff(data, "Li")
#' @export
{
	M        <- ncol(data)
	cor      <- Hmisc::rcorr(as.matrix(data))
	eigen    <- eigen(cor$r, symmetric = T)
	
	if (method == "Cheverud")
	{
		V.lambda <- sum((eigen[[1]] -1)**2)/(M - 1)
		M.eff    <- 1 + (M-1) * (1 - (V.lambda/M))
	}
	else if (method == "Li")
	{
		eigen <- abs(eigen$values)		
		M.eff <- sum(as.numeric(eigen >= 1) + (eigen-floor(eigen)))
	} 
	else 
	{
		stop("Method not valid. Valid methods are 'Cheverud' and 'Li'")
	} 
	
	M.eff
}
