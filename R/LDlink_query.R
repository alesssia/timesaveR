LDpair <- function(rs1, rs2, pop="GBR", LDlinktoken)
#' Fetches LD (r2) between two SNPs
#'
#' Uses LDlink Programmatic Access via API (\url{https://ldlink.nci.nih.gov/?tab=ldpair}).
#' LDlink allows extracting LD between two given SNPs in a given population.
#' According to the CoC, this script should be run sequentially.
#' 
#' @author Alessia Visconti
#' @param rs1 reference SNP IDs for the first variant
#' @param rs2 reference SNP IDs for the second variant
#' @param pop population (multiple populations should be formatted as pop1\%2pop2\%2pop3)
#' @param LDlinktoken personal token (request via \url{https://ldlink.nci.nih.gov/?tab=apiaccess})
#' @return LD between the two given SNPs in the given population
#' @examples
#' LDpair("rs123", "rs456", "GBR", "n0tw0rk1ng")
#' @export
{
	command <- paste0("curl -k -X GET 'https://ldlink.nci.nih.gov/LDlinkRest/ldpair?var1=", rs1, "&var2=", rs2, "&pop=", pop, "&token=", LDlinktoken, "'")
	ld <- system(command, intern = TRUE)[22]
	as.numeric(unlist(strsplit(ld, split=":"))[2])
}

LDproxy <- function(rs, r2=c("r2", "d"), pop="GBR", LDlinktoken, min.r2=0.01, max.distance=500000)
#' Fetches all SNPs in LD with a given SNP
#'
#' Uses LDproxy Programmatic Access via API (\url{https://ldlink.nci.nih.gov/?tab=ldproxy}).
#' LDproxy allows extracting all SNPs in LD with an r2 of at least min.r2 and at a maximun 
#' distance of max.distance. 
#' It processes one SNP at a time.
#' According to the CoC, this script should be run sequentially.
#' 
#' @author Alessia Visconti
#' @param rs reference SNP ID 
#' @param r2 whether look for r2 or D'
#' @param pop population (multiple populations should be formatted as pop1\%2pop2\%2pop3)
#' @param LDlinktoken personal token (request via \url{https://ldlink.nci.nih.gov/?tab=apiaccess})
#' @param min.r2 minum LD (r2) to return 
#' @param max.distance maximun distance (in bp) to return 
#' @return SNPs that are proxies for the given SNP
#' @examples
#' LDproxy("rs123", "r2", "GBR", "n0tw0rk1ng", min.r2=0.8, max.distance=500000)
#' @export
{
	command <- paste0("curl -k -X GET 'https://ldlink.nci.nih.gov/LDlinkRest/ldproxy?var=", rs, "&pop=", pop, "&r2_d=", r2, "&token=", LDlinktoken, "'")
	proxies <- system(command, intern = TRUE)
	proxies <- t(sapply(proxies, function(s) unlist(strsplit(s, split="\t"))))
	rownames(proxies) <- NULL
	colnames(proxies) <- proxies[1, ]
	proxies <- proxies[-1, ]
	proxies <- as.data.frame(proxies[, 1:8])
	proxies[, 4:7] <- apply(proxies[, 4:7], 2, as.numeric)
	proxies[proxies$R2 > min.r2 & abs(proxies$Distance) < max.distance, ]
}
