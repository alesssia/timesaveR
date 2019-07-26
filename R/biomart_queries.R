biomart.fetch.SNP.grch37.mart <- function()
#' Uses biomaRt function to fetch the Ensembl SNP DB
#'
#' Uses biomaRt function to fetch the Ensembl SNP DB (grch37 built, homo sapients)
#'
#' @author Alessia Visconti
#' @return BioMart SNP database (grch37 built, homo sapients)
{
	biomaRt::useMart(biomart="ENSEMBL_MART_SNP", host="grch37.ensembl.org", dataset="hsapiens_snp")
}

biomart.fetch.GENE.grch37.mart <- function()
#' Uses biomaRt function to fetch the Ensembl Gene DB
#'
#' Uses biomaRt function to fetch the Ensembl Gene DB (grch37 built, homo sapients)
#'
#' @return BioMart Gene database (grch37 built, homo sapients)
{
	biomaRt::useMart(biomart = "ENSEMBL_MART_ENSEMBL", host = "grch37.ensembl.org", dataset = "hsapiens_gene_ensembl")
}


biomart.SNPid.in.window <- function(chr, start, end, mart)
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
#' @return reference SNP IDs
#' @examples
#' mybiomart <- biomart.fetch.SNP.grch37.mart()
#' biomart.SNPid.in.window(1, 1, 10100, mybiomart)
{
	m <- biomaRt::getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start'),
	filters = c('chr_name', 'start', 'end'), values = list(chromosome_name=chr, start=start, end=end),  mart = mart)
	m$refsnp_id
}

biomart.gene.in.window <- function(chr, start, end,  mart) 
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
#' @return HUGO gene symbols
#' @examples
#' mybiomart <- biomart.fetch.GENE.grch37.mart()
#' biomart.gene.in.window(1, 1, 20000, mybiomart)
{
	m <- biomaRt::getBM(attributes=c("hgnc_symbol"), filters = c("chromosome_name","start","end"), values=list(chr, start, end), mart = mart)
	m$hgnc_symbol
}


