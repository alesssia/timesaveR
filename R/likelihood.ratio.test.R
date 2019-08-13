likelihood.ratio.test <- function (L0, L1)
#' Evaluates likelihood ratio test
#'  
#' Compares the goodness of fit of a null model (LO) against an alternative model (L1)
#'
#' @author Alessia Visconti
#' @param L0 null model likelihood
#' @param L1 alternative model likelihood
#' @return p value assessing the goodness of fit
#' @examples
#' data(mtcars)
#' model0 <- lm(qsec ~ 1, data=mtcars)
#' model1 <- lm(qsec ~ cyl, data=mtcars)
#' likelihood.ratio.test(logLik(model0), logLik(model1))
#' @export
{
    L01 <- as.vector(- 2 * (L0 - L1))
    pchisq(L01, 1, lower.tail = FALSE)   
}