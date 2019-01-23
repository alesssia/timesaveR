################################################################################
## ESTIMATE EFFECTOVE NUMBER OF TESTS
################################################################################
#' Estimate effective number of tests
#'
#' estimation of effectove number of tests in correlated datasets
#'
#' @author Jonas Zierer
#' @export estimate.meff
#' @references Cheverud JM (2001). A simple correction for multiple comparisons in interval mapping genome scans. Heredity 87: 52â€“58.
#' @param data the data matrix 
#' @return effective number of tests
estimate.meff <- function(data, method = c("Cheverud", "Li")){
	M        <- ncol(data)
	cor      <- Hmisc::rcorr(as.matrix(data))
	eigen    <- eigen(cor$r, symmetric = T)
	#
	if(method == "Cheverud"){
		V.lambda <- sum((eigen[[1]] -1)**2)/(M - 1)
		M.eff    <- 1 + (M-1) * (1 - (V.lambda/M))
	}else if(method == "Li"){
		eigen <- abs(eigen$values)		
		M.eff <- sum(as.numeric(eigen >= 1) + (eigen-floor(eigen)))
	}
	return(M.eff)
}
