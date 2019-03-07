biomart.fetch.SNP.grch37.mart <- function()
# Uses biomaRt function to fetch the Ensembl SNP DB (grch37 built. homo sapients)
#
# Output:
#	BioMart SNP database (grch37 built. homo sapients)
#
{
	biomaRt::useMart(biomart="ENSEMBL_MART_SNP", host="grch37.ensembl.org", dataset="hsapiens_snp")
}

biomart.fetch.GENE.grch37.mart <- function()
# Uses biomaRt function to fetch the Ensembl Gene DB (grch37 built. homo sapients)
#
# Output:
#	BioMart Gene database (grch37 built. homo sapients)
#
{
	biomaRt::useMart(biomart = "ENSEMBL_MART_ENSEMBL", host = "grch37.ensembl.org", dataset = "hsapiens_gene_ensembl")
}


biomart.SNPid.in.window <- function(chr, start, end, mart)
# Uses biomaRt function to fetch the reference SNP IDs for those SNPs included in the 
# specified window and using the given BioMart database
#
# Args:
#	chr		: chromosome number (1, 2, ...)
# 	start	: fist position of the window (included)
#	end		: last position of the window (included)
#	mart	: BioMart database
# Output:
#	reference SNP IDs
#
{
	m <- biomaRt::getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start'),
	filters = c('chr_name', 'start', 'end'), values = list(chromosome_name=chr, start=start, end=end),  mart = mart)
	m$refsnp_id
}

biomart.gene.in.window <- function(chr, start, end,  mart) 
# Uses biomaRt function to fetch the HUGO gene symbols for those genes included in the 
# specified window and using the given BioMart database
#
# Args:
#	chr		: chromosome number (1, 2, ...)
# 	start	: fist position of the window (included)
#	end		: last position of the window (included)
#	mart	: BioMart database
# Output:
#	HUGO gene symbols
#
{
	m <- biomaRt::getBM(attributes=c("hgnc_symbol"), filters = c("chromosome_name","start","end"), values=list(chr, start, end), mart = mart)
	m$hgnc_symbol
}


