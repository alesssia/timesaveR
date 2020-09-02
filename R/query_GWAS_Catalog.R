GWAS.catalog.SNP <- function(rs)
#' Fetches associations in the GWAS Catalog involving the given SNPs
#'
#' Uses GWAS Catalog Access via API (\url{https://www.ebi.ac.uk/gwas/rest/docs/api}).
#' to fetch GWAS associations. If no associations is found, NULL is returned.
#' 
#' @author Alessia Visconti
#' @param rs reference SNP IDs for the variant
#' @return a data frame listing the associations recorded in the GWAS catalog
#' @examples
#' GWAS.catalog.SNP("rs7681615")
#' GWAS.catalog.SNP("rs4693052")
#' @export
{
	#API returns JSON that I need to parse
	url <- paste0("https://www.ebi.ac.uk/gwas/rest/api/associations/search/findByRsId?rsId=", rs)
	association <- jsonlite::fromJSON(txt=url)
	association <- association[[1]]$association
	
	#Checks whether there are any results
	if (is.null(dim(association))) return(NULL)
	
	#Does some formatting and extracts the sub data frames, starting from the
	#infomation on the SNP (loci)
	association$pvalueMantissa <- association$pvalueExponent <- NULL
	loci <- do.call(rbind, lapply(association$loci, function(r) 
	{
		strongestRiskAlleles <- r$strongestRiskAlleles
		strongestRiskAlleles <- lapply(strongestRiskAlleles, function(l) {l$"_links" <- NULL; l})
		strongestRiskAlleles <- do.call(rbind, strongestRiskAlleles)
		apply(strongestRiskAlleles, 2, paste, collapse=";")
	}))
	association$loci <- NULL
		
	#Then extracts information on the trait and the study (more information are 
	#available, but I decided to discard them)
	links <- association$"_links"
	study <- t(sapply(links$study[, 1], function(url) {
		study <- jsonlite::fromJSON(txt=url)
		r <- c(study$diseaseTrait$trait, study$publicationInfo$pubmedId)
		names(r) <- c("trait", "pubmedId")
		r
	}))
	association$"_links" <- NULL
	association <- cbind(loci, study, association)
	rownames(association) <- NULL

	association
}


GWAS.catalog.SNP.proxy <- function(rs, r2=c("r2", "d"), pop="GBR", LDlinktoken, min.r2=0.01, max.distance=500000)
#' Fetches associations in the GWAS Catalog involving the gived all proxy SNP
#'
#' Uses LDproxy Programmatic Access via API (\url{https://ldlink.nci.nih.gov/?tab=ldproxy}),
#' to extract the proxy SNPs, and then uses GWAS Catalog Access via API 
#' (\url{https://www.ebi.ac.uk/gwas/rest/docs/api}) to fetch the GWAS associations.
#' According to the LDLink CoC, this script should be run sequentially.
#' If no associations is found, NULL is returned.
#' 
#' @author Alessia Visconti
#' @param rs reference SNP IDs for the variant
#' @param r2 whether use r2 or D' (use "r2" and "d", respectively)
#' @param pop population (multiple populations should be formatted as pop1\%2pop2\%2pop3)
#' @param LDlinktoken personal token (request via \url{https://ldlink.nci.nih.gov/?tab=apiaccess})
#' @param min.r2 minum LD (r2) to return 
#' @param max.distance maximun distance (in bp) to return 
#' @return a data frame listing the associations recorded in the GWAS catalog
#' @seealso LDproxy
#' @seealso GWAS.catalog.SNP
#' @examples
#' GWAS.catalog.SNP.proxy("rs7681615", "r2", "GBR", "n0tw0rk1ng", 0.8, 500000)
#' GWAS.catalog.SNP.proxy("rs4693052", "r2", "GBR", "n0tw0rk1ng", 0.8, 500000)
#' @export
{
	#Selects proxy SNPs and catch errors
	snps <- tryCatch({ 
		LDproxy(rs, r2, pop, LDlinktoken, min.r2=min.r2, max.distance=max.distance)$RS_Number
	}, warning = function(w) { 
		LDproxy(rs, r2, pop, LDlinktoken, min.r2=min.r2, max.distance=max.distance)$RS_Number
	}, error = function(e) { 
		#If there are errors, I use only the SNP selected
		rs
	})
	
	#Gets the associations
	associations <- lapply(snps, GWAS.catalog.SNP)
	do.call(rbind, associations) 
}
