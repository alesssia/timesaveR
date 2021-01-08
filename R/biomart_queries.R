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


biomart.SNPid.in.window <- function(chr, start, end, mart, with.position=FALSE, with.alleles=FALSE) 
#' Fetches the reference SNP IDs in the spefified window
#'
#' Uses biomaRt function to fetch the reference SNP IDs for those SNPs included in the 
#' specified window. It uses the given BioMart database.
#'
#' @author Alessia Visconti
#' @param chr chromosome number (1, 2, ...)
#' @param start fist position of the window (included)
#' @param end last position of the window (included)
#' @param mart BioMart database
#' @param with.position whether including chromosome name and start and end position (default: false)
#' @param with.alleles whether including variant alleles (default: false, if with.position==TRUE is forced to TRUE)
#' @return reference SNP IDs, or reference SNP IDs plus genomic coordinates and alleles
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' biomart.SNPid.in.window(1, 1, 10100, mybiomart)
#' biomart.SNPid.in.window(1, 1, 10100, mybiomart, with.position=TRUE)
#' biomart.SNPid.in.window(1, 1, 10100, mybiomart, with.alleles=TRUE)
#' @export
{
	#Query
	m <- biomaRt::getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start', 'allele'), filters = c('chr_name', 'start', 'end'), values = list(chromosome_name=chr, start=start, end=end),  mart = mart)
	colnames(m) <- c("SNP", "CHR", "BP", "Allele")
	
	#Selects attributes
	if (with.alleles) {
		return(m)
	} else if (with.position) {
		return(m[, c("SNP", "CHR", "BP")])
	} else {
		return(m$SNP)
	}
	
}

biomart.SNP.position <- function(rs, mart, with.alleles=FALSE) 
#' Fetches the chromosomal coordinates of the specified SNP IDs 
#'
#' Uses biomaRt function to fetch the chromosomal coordinates for the given 
#' SNP IDs It uses the given BioMart database.
#'
#' @author Alessia Visconti
#' @param rs the variant mame (rs ID, ...)
#' @param mart BioMart database
#' @param with.alleles whether including variant alleles (default: FALSE)
#' @return reference SNP IDs with its genomic coordinates, or reference SNP IDs with its genomic coordinate and alleles
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' biomart.SNP.position("rs9890579", mybiomart)
#' biomart.SNP.position("rs9890579", mybiomart, with.alleles=TRUE)
#' @export
{
	#Query
	m <- biomaRt::getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start', 'allele'), filters = c('snp_filter'), values = list(snp_filter=rs),  mart = mart)
	colnames(m) <- c("SNP", "CHR", "BP", "Allele")
	
	#Selects attributes
	if (with.alleles) {
		return(m)
	} else {
		return(m[, c("SNP", "CHR", "BP")])
	}
	
}


biomart.SNP.rsID <- function(chr, ps, allele0, allele1, mart, with.position=FALSE, with.alleles=FALSE)
#' Fetches the rs SNP IDs given the chromosomal coordinates and alleles
#'
#' Uses biomaRt function to fetch the rs SNP ID for the given 
#' chromosoma coordinates and alleles. If no SNP is found in that position with those alleles
#' NA is returned. It uses the given BioMart database.
#'
#' @author Alessia Visconti, Niccolo' Rossi
#' @param chr chromosome number (1, 2, ...)
#' @param ps position (bp)
#' @param allele0 alternate allele
#' @param allele1 reference allele
#' @param mart BioMart database
#' @param with.position whether including chromosome name and start and end position (default: false)
#' @param with.alleles whether including variant alleles (default: false, if with.position==TRUE is forced to TRUE)
#' @return reference SNP IDs, or reference SNP IDs plus genomic coordinates and alleles
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' biomart.SNP.rsID(7, 24966446, "C", "A", mybiomart)
#' biomart.SNP.rsID(17, 75020291, "A", "G", mybiomart)
#' biomart.SNP.rsID(7, 24966446, "C", "A", mybiomart, with.position=TRUE)
#' biomart.SNP.rsID(17, 75020291, "A", "G", mybiomart, with.alleles=TRUE)
#' biomart.SNP.rsID(5, 70119760, "A", "G", mybiomart, with.alleles=TRUE)
#' @export
{
	#Gets the alleles combination to look for
	biallelic <- paste0("^", allele0, "/", allele1, "|", "^", allele1, "/", allele0)
	multiallelic <-  paste0("^", allele0, "/*/", allele1, "|", "^", allele1, "/*/", allele0)
	alleles <- paste(biallelic, multiallelic, sep="|")
	
	#Fetches SNPs and selects the correct one based on alleles
	m <- biomart.SNPid.in.window(chr, ps, ps, mart, with.alleles=T)
	m <- m[grepl(glob2rx(alleles), m$Allele), ]
		
	#No SNP in that position with the given alleles
	if (nrow(m) == 0) return(NA)
	
	#Selects attributes
	if (with.alleles) {
		return(m)
	} else if (with.position) {
		return(m[, c("SNP", "CHR", "BP")])
	} else {
		return(m$SNP)
	}
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


biomart.gene.position <- function(gene, mart) 
#' Fetches the chromosomal coordinates of the specified gene 
#'
#' Uses biomaRt function to fetch the chromosomal coordinates for the given 
#' HUGO gene symbols. It uses the given BioMart database.
#'
#' @author Alessia Visconti
#' @param gene the variant mame (HUGO gene symbols)
#' @param mart BioMart database
#' @return genomic coordinates
#' @examples
#' mybiomart <- biomart.fetch.GENE.grch37.mart()
#' biomart.gene.position("DDX11L1", mybiomart)
#' @export
{
	#Query
	m <- biomaRt::getBM(attributes= c("chromosome_name", "start_position", "end_position"), filters = c("hgnc_symbol"), values=list(gene), mart = mart)
	colnames(m) <- c("CHR", "START", "END")
	m
}
