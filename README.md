<<<<<<< Updated upstream
### Short description
Text
https://docs.google.com/document/d/1Q6DZPjqN2XVzwZaoQhyoi6vsYZK8LEKFahtVouEEOk0/edit?usp=drive_link
=======
# PMScanR <img src="inst/img/PMlogo_3D.png" align="right" height = 150/>

![GitHub last commit](https://img.shields.io/github/last-commit/prodakt/PMScanR)
![GitHub R package version](https://img.shields.io/github/r-package/v/prodakt/PMScanR)
![GitHub License](https://img.shields.io/github/license/prodakt/PMScanR)

`PMScanR` is an R package for the large-scale identification, analysis, and visualization of protein motifs. It integrates PROSITE's `ps_scan` tool, handles data conversion, and offers multiple visualization methods, including heatmaps and sequence logos. The package also features a full graphical user interface (GUI) via Shiny for a code-free experience.

## Installation

Once the package is accepted to Bioconductor, you will install it by starting R (version "4.4" or higher) and entering:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("PMScanR")
```

## Quick Start

You can either launch the interactive application or run an analysis via the command line.

#### Interactive GUI

For a user-friendly, clickable interface that covers the entire analysis workflow, simply run:

```r
library(PMScanR)
runPMScanRShiny()
```
![GUI Screenshot](https://github.com/prodakt/PMScanR/blob/main/inst/img/PMScanR_1small.png)


#### Command Line Example

```r
library(PMScanR)

# 1. Get path to the example FASTA file included with the package
fasta_file <- system.file("extdata", "hemoglobins.fasta", package = "PMScanR")

# 2. Run the motif scan and save results in GFF format
runPsScan(in_file = fasta_file, out_format = 'gff', out_file = "results_scan.gff")

# For a full, step-by-step guide, please see the package vignette:
browseVignettes("PMScanR")
```
>>>>>>> Stashed changes
