LDpair <- function(rs1, rs2, pop="GBR", LDlinktoken)
# Uses LDlink Programmatic Access via API (https://ldlink.nci.nih.gov/?tab=ldpair).
# LDlink allows extracting LD between two given SNPs in a given population.
# According to the CoC, this script should be run sequentially.
# 
# Args:
#	rs1		: reference SNP IDs for the first variant
#	rs2		: reference SNP IDs for the second variant
#	pop 	: population (multiple populations should be formatted as pop1%2pop2%2pop3)
# 	LDlinktoken : personal token (request via https://ldlink.nci.nih.gov/?tab=apiaccess)
# Output:
#	LD between the two given SNPs in the given population
#
{
	command <- paste0("curl -k -X GET 'https://ldlink.nci.nih.gov/LDlinkRest/ldpair?var1=", rs1, "&var2=", rs2, "&pop=", pop, "&token=", LDlinktoken, "'")
	ld <- system(command, intern = TRUE)[22]
	as.numeric(unlist(strsplit(ld, split=":"))[2])
}

LDproxy <- function(rs, r2=c("r2", "d"), pop="GBR", LDlinktoken)
# Uses LDproxy Programmatic Access via API (https://ldlink.nci.nih.gov/?tab=ldproxy).
# LDproxy allows extracting all SNPs in LD with the following criteria:
#	- min r2 = 0.01
#	- max distance 500kb
# According to the CoC, this script should be run sequentially.
# 
# Args:
#	rs		: reference SNP IDs 
#	r2		: whether look for r2 or D'
#	pop 	: population (multiple populations should be formatted as pop1%2pop2%2pop3)
# 	LDlinktoken : personal token (request via https://ldlink.nci.nih.gov/?tab=apiaccess)
# Output:
#	SNPs that are proxies for the given SNP
#
{
	command <- paste0("curl -k -X GET 'https://ldlink.nci.nih.gov/LDlinkRest/ldproxy?var=", rs, "&pop=", pop, "&r2_d=", r2, "&token=", LDlinktoken, "'")
	proxies <- system(command, intern = TRUE)
	proxies <- t(sapply(proxies, function(s) unlist(strsplit(s, split="\t"))))
	rownames(proxies) <- NULL
	colnames(proxies) <- proxies[1, ]
	proxies <- proxies[-1, ]
	proxies <- as.data.frame(proxies[, 1:8])
	proxies[, 4:7] <- apply(proxies[, 4:7], 2, as.numeric)
	proxies
}
