likelihood.ratio.test <- function (L0, L1)
# Compares the goodness of fit of a null model (LO) against an alternative model (L1)
#
# Args:
#	LO	:  null model likelihood
#	L1	:  alternative model likelihood
# Output:
# 	p value assessing the goodness of fit
#
{
    L01 <- as.vector(- 2 * (L0 - L1))
    pchisq(L01, 1, lower.tail = FALSE)   
}