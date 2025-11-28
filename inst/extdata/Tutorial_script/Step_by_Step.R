# Tutorial Script for PMScanR Package: Step-by-Step Guide to Motif Scanning and Visualization
# This script provides a step-by-step guide to using PMScanR package for protein motif scanning,
# format conversion, matrix creation, and visualization.
# Users can run this script section by section to understand and apply the functionalities
# of the PMScanR package.

# --- Section 1: Setup -----------
# This section prepares your R environment by installing necessary packages

# Install BiocManager and seqLogo
# The 'seqLogo' package is from Bioconductor and is required for generating sequence logos.
# BiocManager is used to install packages from Bioconductor.
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Install 'seqLogo' and PMScanR packages from Bioconductor using BiocManager
BiocManager::install("PMScanR")
# --- Section 2: Package and Library Loading -----------
# This section loads all the necessary R libraries that will be used in this tutorial.
# We will load libraries for package development, data manipulation, file handling,
# visualization, and sequence analysis.

# 2.1 Load Required Libraries
# We will use 'library()' function to load each required package.

library(devtools)    # For package development and loading
library(dplyr)       # For data manipulation
library(rtracklayer) # For handling GFF files
library(reshape2)    # For data reshaping, especially for heatmaps
library(plotly)      # For interactive plots
library(seqLogo)     # For sequence logo generation
library(ggseqlogo)  # For more advanced sequence logo generation
library(ggplot2)     # For general plotting
library(seqinr)      # For reading and handling biological sequences (FASTA files)


# --- Section 3: Loading Needed Files for Analysis ---
# This section defines file paths to external tools (PS-Scan), data files (FASTA sequences,
# PROSITE patterns), and specifies output formats and filenames.
# Ensure that the 'ps_scan' directory and 'data' directory are in your working directory,
# or adjust the paths accordingly.

# 3.1 Define File Paths for PS-Scan and Data
# Define variables for the paths to PS-Scan perl script, PROSITE database,
# output format, PFSCAN executable, output file, and input FASTA file.
out_format <-
    "psa"                    # Default output format for PS-Scan (PSA - PROSITE scan ASCII)
out_file <-
    "out_Hb_psa.txt"           # Default output filename for PSA format results (e.g., 'out_Hb_psa.txt')
in_file <-
    system.file("extdata", "hemoglobins.fasta", package = "PMScanR")   # Path to the input FASTA file (example: hemoglobin sequences)

# --- Section 4: Running PS-Scan for Motif Scanning ---
# This section demonstrates how to use functions in PMScanR to run PS-Scan
# Example 1: Running runPsScan() with requaired attributes.
# Without specifing the rest of attributes required files will be downloaded by default from prosite page,
# and user will be prompted to confirm that dtetected OS is correct
runPsScan(in_file = in_file,
          out_format = 'gff',
          out_file = "results_pfscan.gff")

# --- Section 5: File Format Conversion from PSA or PROSITE to GFF ---
# This section demonstrates how to convert PS-Scan output files from PSA or PROSITE format
# to GFF (General Feature Format) using the 'read.psa()' or 'read.prosite()'function from PMScanR package.
# GFF format is widely used in bioinformatics and is compatible with many tools,
# including genome browsers and data analysis packages.

# 5.1 Converting PSA Format to GFF using read.psa()
# The 'read.psa()' function takes a PS-Scan PSA format output file as input
# and returns a data frame in a GFF-like format, suitable for further analysis in R.

# Example 5.1.1: Converting a PSA output file (using a filename variable)
motifs_psa <-
    system.file("extdata",
                "out_Hb_psa.txt",
                package = "PMScanR",
                mustWork = TRUE)
psaGFF <- read.psa(motifs_psa)
head(psaGFF) # Display the first few rows of the converted GFF-like data frame

# Example 5.1.2: Converting a PSA output file (using a relative path directly)
psaGFF <-
    read.psa(".../out_Hb_psa.txt") # Using a relative path to PSA file
head(psaGFF)

# 5.2 Converting PROSITE Format to GFF using read.prosite()
# The 'read.prosite()' function takes a PS-Scan PROSITE format output file as input
# and returns a data frame in a GFF-like format, suitable for further analysis in R.

# Example 5.2.1: Converting a PROSTIE output file (using a filename variable)
motifs_prosite <-
    system.file("extdata",
                "PROSITEoutput.txt",
                package = "PMScanR",
                mustWork = TRUE)
prositeGFF <- read.prosite(motifs_prosite)
head(prositeGFF) # Display the first few rows of the converted GFF-like data frame

# Example 5.2.2: Converting a PROSITE output file (using a relative path directly)
prositeGFF <-
    read.prosite(".../PROSITEoutput.txt") # Using a relative path to PSA file
head(prositeGFF)

# --- Section 6: Reading GFF Format Files Directly ---
# If you have generated PS-Scan output directly in GFF format (using 'out_format = "gff"'),
# or if you have other GFF format files containing motif information, you can read them
# directly into R using 'rtracklayer::import.gff()'.

# 6.1 Reading GFF format using rtracklayer::import.gff()
# The 'import.gff()' function from the 'rtracklayer' package is a powerful tool
# for reading GFF, GTF, and other genomic annotation files.

# Example 6.1.1: Reading a GFF format file (using filename variable)
gff_format <-
    rtracklayer::import.gff("nopfGFF.txt") # Reading GFF format file
head(gff_format) # Display the first few lines of the imported GFF object

# Example 6.1.2: Reading a GFF format file (using different output file from PS-Scan)
gff_format <-
    rtracklayer::import.gff("out_Hb_gff.txt") # Reading a different GFF output file
head(gff_format)


# --- Section 7: Creating Motif Occurrence Matrix from GFF Data ---
# This section shows how to convert the GFF-formatted motif data (either from PSA conversion
# or direct GFF input) into a motif occurrence matrix. In this matrix, rows represent sequences
# and columns represent motifs. The cells contain binary values (1 or 0) indicating whether a motif
# is found in a sequence.

# 7.1 Creating Motif Occurrence Matrix using gff2matrix()
# The 'gff2matrix()' function in PMScanR takes GFF-formatted data (either as a data frame
# from 'read.psa()' or an imported GFF object) and creates a motif occurrence matrix.
# from 'read.prosite()' or an imported GFF object) and creates a motif occurrence matrix.

# Example 7.1.1: Creating matrix from PSA-converted GFF data
mom <-
    gff2matrix(psaGFF) # 'psaGFF' is the GFF-like data frame from PSA conversion
head(mom) # Display the first few rows of the Motif Occurrence Matrix (MOM)

# Example 7.1.2: Creating matrix from directly read GFF format data
momGFF <-
    gff2matrix(as.data.frame(gff_format)) # 'gff_format' is the GFF object imported by rtracklayer
head(momGFF) # Display the first few rows of the MOM from direct GFF input

# Example 7.1.3: Creating matrix from PROSITE-converted GFF data
mom <-
    gff2matrix(prositeGFF) # 'prositeGFF' is the GFF-like data frame from PROSITE conversion
head(mom) # Display the first few rows of the Motif Occurrence Matrix (MOM)

# 7.2 Saving Motif Occurrence Matrix to CSV file
# For further analysis, visualization, or sharing, you can save the generated
# Motif Occurrence Matrix to a CSV (Comma Separated Values) file using 'write.csv2()'.

write.csv2(mom, "MotifsOccurenceMatrix.csv") # Save the MOM to 'MotifsOccurenceMatrix.csv' file


# --- Section 8: Heatmap Visualization of Motif Occurrence Matrix ---
# This section demonstrates how to visualize the Motif Occurrence Matrix using heatmaps.
# Heatmaps are useful for visualizing patterns of motif occurrences across sequences.
# We will use functions 'matrix2hm()' and 'matrix2hm_2()' (assuming they are part of PMScanR)
# to generate heatmaps with different styles.

# 8.1 Heatmap using matrix2hm() - from PSA-converted Matrix
# The 'matrix2hm()' function (assumed from PMScanR) generates a heatmap from a motif occurrence matrix.
# This example creates a heatmap from the MOM derived from PSA-converted GFF data ('mom').
hm1 <-
    matrix2hm(x = colnames(mom),
              # 'x' argument specifies columns (motifs) to be shown on heatmap's x-axis
              y = row.names(mom),
              # 'y' argument specifies rows (sequences) to be shown on heatmap's y-axis
              input = mom)           # 'input' argument takes the Motif Occurrence Matrix
hm1 # Display the heatmap 'hm1'

# 8.2 Heatmap using matrix2hm() - from Directly Read GFF Matrix
# This example creates a heatmap using 'matrix2hm()' but from the MOM derived from directly read GFF data ('momGFF').
hmgff <- matrix2hm(x = colnames(momGFF),
                   y = row.names(momGFF),
                   input = momGFF)
hmgff # Display the heatmap 'hmgff'


# 8.3 Heatmap using matrix2hm_2() - from PSA-converted Matrix
# The 'matrix2hm_2()' function (assumed from PMScanR) is another function for heatmap generation,
# possibly with a different style or options compared to 'matrix2hm()'.
# This example uses 'matrix2hm_2()' to visualize the MOM from PSA-converted data ('mom').
hm2 <- matrix2hm_2(x = colnames(mom),
                   y = row.names(mom),
                   input = mom)
hm2 # Display heatmap 'hm2'

# 8.4 Heatmap using matrix2hm_2() - from Directly Read GFF Matrix (subset of rows)
# When dealing with a large number of sequences, it might be useful to visualize a subset of rows
# to make the heatmap more readable. This example shows how to create a heatmap of the first 10 sequences
# from the MOM derived from directly read GFF data ('momGFF') using 'matrix2hm_2()'.
hm2gff <-
    matrix2hm_2(x = colnames(momGFF),
                # Motifs on x-axis
                y = row.names(momGFF)[1:10],
                # First 10 sequences on y-axis
                input = momGFF[c(1:10), ])             # Input matrix - first 10 rows
hm2gff # Display heatmap 'hm2gff'


# --- Section 9: Sequence Logo Generation ---
# This section demonstrates how to generate sequence logos, which are graphical representations
# of the sequence conservation of motifs or regions of interest. We will show how to create
# sequence logos from raw sequences (FASTA file) and from identified motifs (PSA/GFF output).

# 9.1 Sequence Logo from Raw Sequences (FASTA file) using ggseqlogo
# 'ggseqlogo' package is used to create sequence logos. To create a logo from raw sequences,
# you first need to extract a region of interest from your FASTA sequences.

# Example 9.1.1: Creating sequence logo for a selected region from FASTA
# Define the start and end position of the region you want to visualize as a sequence logo.
from_pos <- 10 # Starting position of the region
to_pos <- 20   # Ending position of the region

seq <-
    read.fasta(file = in_file, seqtype = "AA") # Read the FASTA file containing protein sequences
seqShort <-
    extract_segments(seq = seq, from_pos, to_pos)          # Extract segments from position 'from_pos' to 'to_pos' for all sequences
ggseqlogo(unlist(seqShort), seq_type = "aa")                       # Generate and display the sequence logo of the extracted segments

# 9.2 Sequence Logo from Identified Motifs (PSA/GFF output) using ggseqlogo
# To create sequence logos for identified motifs, you first need to extract the motif sequences
# from the PSA or GFF format output using a function like 'extract_protein_motifs()' (assumed from PMScanR).

# Example 9.2.1: Creating sequence logos for motifs from PSA output
# First, extract protein motif sequences from a PSA format file using 'extract_protein_motifs()'.
protein_motifs_psa <-
    extract_protein_motifs(motifs_psa) # or use 'out_Hb_psa.txt' filename

# Now, you can generate sequence logos for specific motifs from 'protein_motifs_psa'.
ggseqlogo(protein_motifs_psa$PS60007, seq_type = 'aa') # Sequence logo for motif 'PS60007' (example motif ID)
ggseqlogo(protein_motifs_psa[1], seq_type = 'aa')       # Sequence logo for the first motif in the list
ggseqlogo(protein_motifs_psa[5], seq_type = 'aa')       # Sequence logo for the fifth motif in the list

# --- Section 10: Shiny app run ---
# If you want you can use shiny to use all the features of the package with user friendly UI helping to follow all the above steps
# To run Shiny app you can call function runPMScanRShiny()
runPMScanRShiny()

# --- End of PMScanR Tutorial Script ---
