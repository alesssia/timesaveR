biomart.fetch.SNP.grch37.mart <- function()
#' Uses biomaRt function to fetch the Ensembl SNP DB
#'
#' Uses biomaRt function to fetch the Ensembl SNP DB (grch37 built, homo sapients)
#'
#' @author Alessia Visconti
#' @return BioMart SNP database (grch37 built, homo sapients)
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' @export
{
	biomaRt::useMart(biomart="ENSEMBL_MART_SNP", host="grch37.ensembl.org", dataset="hsapiens_snp")
}

biomart.fetch.GENE.grch37.mart <- function()
#' Uses biomaRt function to fetch the Ensembl Gene DB
#'
#' Uses biomaRt function to fetch the Ensembl Gene DB (grch37 built, homo sapients)
#'
#' @return BioMart Gene database (grch37 built, homo sapients)
#' @examples
#' mybiomart <- biomart.fetch.GENE.grch37.mart()
#' @export
{
	biomaRt::useMart(biomart = "ENSEMBL_MART_ENSEMBL", host = "grch37.ensembl.org", dataset = "hsapiens_gene_ensembl")
}


biomart.SNPid.in.window <- function(chr, start, end, mart, with.position=FALSE) 
#' Fetches the reference SNP IDs in the spefified window
#'
#' Uses biomaRt function to fetch the reference SNP IDs for those SNPs included in the 
#' specified window. It uses the given BioMart database
#'
#' @author Alessia Visconti
#' @param chr chromosome number (1, 2, ...)
#' @param start fist position of the window (included)
#' @param end last position of the window (included)
#' @param mart BioMart database
#' @param with.position whether included chromosome name and start and end position (default: false)
#' @return reference SNP IDs, or reference SNP IDs plus genomic coordinates 
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' biomart.SNPid.in.window(1, 1, 10100, mybiomart)
#' biomart.SNPid.in.window(1, 1, 10100, mybiomart, with.position=TRUE)
#' @export
{
	#Query
	m <- biomaRt::getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start'), filters = c('chr_name', 'start', 'end'), values = list(chromosome_name=chr, start=start, end=end),  mart = mart)
	
	#Selects attributes
	if (with.position) {
		colnames(m) <- c("SNP", "CHR", "BP")
		return(m)
	} else {
		return(m$refsnp_id)
	}
}

biomart.SNP.position <- function(rs, mart) 
#' Fetches the chromosomal coordinates of the specified SNP IDs 
#'
#' Uses biomaRt function to fetch the chromosomal coordinates for the given 
#' SNP IDs It uses the given BioMart database.
#'
#' @author Alessia Visconti
#' @param rs the variant mame (rs ID, ...)
#' @param mart BioMart database
#' @return reference SNP IDs with its plus genomic coordinates 
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' biomart.SNP.position("rs9890579", mybiomart)
#' @export
{
	#Query
	m <- biomaRt::getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start'), filters = c('snp_filter'), values = list(snp_filter=rs),  mart = mart)
	colnames(m) <- c("SNP", "CHR", "BP")
	m
}


biomart.gene.in.window <- function(chr, start, end,  mart, with.position=FALSE) 
#' Fetches the HUGO gene symbols in the spefified window
#'
#' Uses biomaRt function to fetch the HUGO gene symbols for those genes included in the 
#' specified window. It uses the given BioMart database
#'
#' @author Alessia Visconti
#' @param chr	chromosome number (1, 2, ...)
#' @param start fist position of the window (included)
#' @param end last position of the window (included)
#' @param mart BioMart database
#' @param with.position whether included chromosome name and start and end position (default: false)
#' @return HUGO gene symbols, or HUGO gene symbols plus gene boudaries
#' @examples
#' mybiomart <- biomart.fetch.GENE.grch37.mart()
#' biomart.gene.in.window(1, 1, 20000, mybiomart)
#; biomart.gene.in.window(1, 1, 20000, mybiomart, with.position=TRUE)
#' @export
{
	#Query
	m <- biomaRt::getBM(attributes= c("hgnc_symbol", "chromosome_name", "start_position", "end_position"), filters = c("chromosome_name","start","end"), values=list(chr, start, end), mart = mart)
	
	#Selects attributes
	if (with.position) {
		colnames(m) <- c("GENE", "CHR", "START", "END")
		return(m)
	} else {
		return(m$hgnc_symbol)
	}
}


