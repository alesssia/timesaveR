LDpair <- function(rs1, rs2, pop="GBR", LDlinktoken)
#' Fetches LD (r2) between two SNPs
#'
#' Uses LDlink Programmatic Access via API (\url{https://ldlink.nci.nih.gov/?tab=ldpair}).
#' LDlink allows extracting LD between two given SNPs in a given population.
#' According to the LDLink CoC, this script should be run sequentially.
#' 
#' @author Alessia Visconti
#' @param rs1 reference SNP IDs for the first variant
#' @param rs2 reference SNP IDs for the second variant
#' @param pop population (multiple populations should be formatted as pop1\%2pop2\%2pop3)
#' @param LDlinktoken personal token (request via \url{https://ldlink.nci.nih.gov/?tab=apiaccess})
#' @return LD between the two given SNPs in the given population, or NA if one or both of the SNPs is not found
#' @examples
#' LDpair("rs123", "rs456", "GBR", "n0tw0rk1ng")
#' @export
{
	command <- paste0("curl -k -X GET 'https://ldlink.nci.nih.gov/LDlinkRest/ldpair?var1=", rs1, "&var2=", rs2, "&pop=", pop, "&token=", LDlinktoken, "'")
	ld <- system(command, intern = TRUE, ignore.stderr=TRUE)[22]
	as.numeric(unlist(strsplit(ld, split=":"))[2])
}

LDproxy <- function(rs, r2=c("r2", "d"), pop="GBR", LDlinktoken, min.r2=0.01, max.distance=500000)
#' Fetches all SNPs in LD with a given SNP
#'
#' Uses LDproxy Programmatic Access via API (\url{https://ldlink.nci.nih.gov/?tab=ldproxy}).
#' LDproxy allows extracting all SNPs in LD with an r2 of at least min.r2 and at a maximun 
#' distance of max.distance. 
#' It processes one SNP at a time.
#' According to the LDLink CoC, this script should be run sequentially.
#' 
#' @author Alessia Visconti
#' @param rs reference SNP ID 
#' @param r2 whether use r2 or D' (use "r2" and "d", respectively)
#' @param pop population (multiple populations should be formatted as pop1\%2pop2\%2pop3)
#' @param LDlinktoken personal token (request via \url{https://ldlink.nci.nih.gov/?tab=apiaccess})
#' @param min.r2 minum LD (r2) to return 
#' @param max.distance maximun distance (in bp) to return 
#' @return SNPs that are proxies for the given SNP. If no SNPs is identified a data frame populated by NA is returned instead
#' @examples
#' LDproxy("rs123", "r2", "GBR", "n0tw0rk1ng", min.r2=0.8, max.distance=500000)
#' @export
{
	command <- paste0("curl -k -X GET 'https://ldlink.nci.nih.gov/LDlinkRest/ldproxy?var=", rs, "&pop=", pop, "&r2_d=", r2, "&token=", LDlinktoken, "'")
	proxies <- system(command, intern = TRUE, ignore.stderr=TRUE)
	
	#Manages one of the errors returned by LDLink
	if (grepl("error", proxies[[2]])) 
	{
		message <- trimws(gsub(paste0(unlist(strsplit(proxies[[2]], split=":"))[-1], collapse=":"),  pattern="\"", replacement=""))
		print(paste(rs, ":", message))
		proxies <- data.frame(Query=rs, RS_Number=rs, Coord=NA, Alleles=NA, MAF=NA, Distance=NA, Dprime=NA,     R2=NA, Correlated_Alleles=NA)
		return(proxies)
	}
			
	proxies <- t(sapply(proxies, function(s) unlist(strsplit(s, split="\t"))))
	rownames(proxies) <- NULL
	colnames(proxies) <- proxies[1, ]
	proxies <- proxies[-1, ]
	proxies <- data.frame(Query=rs, proxies[, 1:8])
	proxies[, c("MAF", "Distance", "Dprime", "R2")] <- apply(proxies[, c("MAF", "Distance", "Dprime", "R2")], 2, as.numeric)
	proxies[proxies$R2 > min.r2 & abs(proxies$Distance) < max.distance, ]
}

LDtrait <- function(rs, r2=c("r2", "d"), pop="GBR", LDlinktoken, min.r2=0.1)
#' Fetches all SNPs in LD with a given SNP
#'
#' Uses LDproxy Programmatic Access via API (\url{https://ldlink.nci.nih.gov/?tab=ldproxy}).
#' LDproxy allows extracting all SNPs in LD with an r2 of at least min.r2 and at a maximun 
#' distance of max.distance. 
#' It processes one SNP at a time.
#' According to the LDLink CoC, this script should be run sequentially.
#' 
#' @author Alessia Visconti
#' @param rs reference SNP ID 
#' @param r2 whether use r2 or D' (use "r2" and "d", respectively)
#' @param pop population (multiple populations should be formatted as pop1\%2pop2\%2pop3)
#' @param LDlinktoken personal token (request via \url{https://ldlink.nci.nih.gov/?tab=apiaccess})
#' @param min.r2 minum LD (r2) to return 
#' @return known variants in GWAS Catalog (including those in LD), If no variant is identified a data frame populated by NA is returned instead
#' @examples
#' LDtrait("rs123", "r2", "GBR", "n0tw0rk1ng", min.r2=0.1)
#' @export
{
	cols <- c("Query", "GWAS_trait", "PMID", "RS_Number", "Position_GRCh37", "Alleles", "R2", "Dprime", "Risk_Allele_Frequency", "Effect_Size_95%_CI", "Beta_or_OR", "P_value")
	
	command <- paste0("curl -k -H \"Content-Type: application/json\" -X POST -d '{\"snps\": \"", rs, "\", \"pop\": \"", pop, "\", \"r2_d\": \"", r2, "\", \"r2_d_threshold\": \"", min.r2, "\", \"genome_build\": \"grch37\"}' 'https://ldlink.nih.gov/LDlinkRest/ldtrait?token=", LDlinktoken, "'")
	
	r <- system(command, intern = TRUE, ignore.stderr=TRUE)
		
	#Manages one of the errors returned by LDLink
	if (grepl("error", r[[2]])) 
	{
		message <- trimws(gsub(unlist(strsplit(r[[2]], split=":"))[2], pattern="\"", replacement=""))
		print(paste(rs, ":", message))
		r <- t(data.frame(c(rs, rep(NA, 11))))
		rownames(r) <- NULL
		colnames(r) <- cols
		return(r)
	}
	
	r <- as.data.frame(t(sapply(r, function(s) unlist(strsplit(s, split="\t")))))
	rownames(r) <- NULL
	colnames(r) <- cols
	r <- r[-1, ]
	
	r$Risk_Allele_Frequency[r$Risk_Allele_Frequency == "NR"] <- NA
	r[, c("R2", "Dprime", "Risk_Allele_Frequency", "P_value")] <- apply(r[, c("R2", "Dprime", "Risk_Allele_Frequency", "P_value")], 2, as.numeric)
	
	r
}
