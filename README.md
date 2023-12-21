# timesaveR

My daily work mostly entails running the same data cleaning, pre- and post-processing functions over and over again. For weeks, and weeks. Months. Years. 

This rough R package includes a collections of functions doing the completely different things, their only common denominator is that **they save me time**. 


## Installation


```r
devtools::install_github("alesssia/timesaveR")
```

### Dependencies

* R: https://www.r-project.org/
* devtools: https://github.com/r-lib/devtools
* Functions calculating linkage disequilibrium statistics query the [LDproxy Programmatic Access](https://ldlink.nci.nih.gov/?tab=home) via [API](https://ldlink.nci.nih.gov/?tab=apiaccess). This requires a LDlink personal token, that can be requested  [here](https://ldlink.nci.nih.gov/?tab=apiaccess).


## License

timesaveR is licensed under GNU GPL v3.


## Acknowledgements 

While some of the functions have been developed 100% by me, others are liberal (or not so liberal) adaptations of other people's work, several of whom I had lost track. I am happy to include attribution whenever I can: if you spot your or someone else's work, please let me know.


## Changelog

### 0.0.1.6 / 2023-12-21

Enhancements:
* Added a second function (`LDtrait`) that queries the [GWAS Catalog](https://www.ebi.ac.uk/gwas/home) this time *via* the [LDproxy Programmatic Access](https://ldlink.nci.nih.gov/?tab=home)
* `LDproxy` returns also the queried SNP (rsID)
* All functions based on the [LDproxy Programmatic Access](https://ldlink.nci.nih.gov/?tab=home) are now suppressing messages written to `stderr`.


### 0.0.1.5 / 2023-08-04

Enhancements:
* Improved `inverse.normal`
* Improved handling of errors/warning in `LDproxy`
* `estimate.heritability` now checks the confidence intervals when extracting the best model

### 0.0.1.4 / 2020-09-02

Enhancements:
* Improved handling of errors when linkage disequilibrium statistics are used
* `biomart.SNP.position` can now return also the variant alleles
* Added a function (`biomart.SNP.rsID`) that returns the rsID given the chromosomal coordinates and the alleles

### 0.0.1.3 / 2020-02-05

Enhancements:
* Functions `biomart.SNPid.in.window` can now return also the variant alleles

### 0.0.1.2 / 2019-10-25

Enhancements:
* Functions `biomart.*.in.window` can now return also the chromosomal coordinates
* Added a function (`biomart.SNP.position`) that returns the chromosomal coordinates of a given SNP
* Added functions to query the [GWAS Catalog](https://www.ebi.ac.uk/gwas/home) (`GWAS.catalog.SNP` and `GWAS.catalog.SNP.proxy`)


