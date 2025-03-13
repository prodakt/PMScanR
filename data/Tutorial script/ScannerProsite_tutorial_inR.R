if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("seqLogo")


load_all()

# Loading a needed libraries
library(devtools)
library(dplyr)
library(rtracklayer)
library(reshape2)
library(plotly)
library(seqLogo)
library(ggseqlogo)
library(ggplot2)
library(seqinr)
library(shiny)

load_packages("shiny")

# Loading the needed files for analysis

# For running PS-Scan on Windows and Linux
ps_scan <- "ps_scan.pl"
patterns_dat <- "prosite.dat"
out_format <- "psa"
pf_scan <- "pfscan_temp/ps_scan/pfscan.exe"
out_file <- "data/out_Hb_psa.txt"
in_file <- "data/hemoglobins.fasta"
debug(runPsScan)
runPsScan(in_file = in_file, out_file = out_file, out_format = "psa")

1# Running PS-Scan on Windows
runPsScan_Win(in_file = in_file, out_file = out_file, out_format = out_format,
              ps_scan = ps_scan, patterns_dat = patterns_dat, pf_scan = pf_scan)

# You can give the file format paths in the arguments or manually in the code
runPsScan_Win(in_file = in_file,
              out_file = "out_Hb_gff.txt", out_format = "gff",
              ps_scan = ps_scan, patterns_dat = patterns_dat, pf_scan = pf_scan)


runPsScan_Win(in_file = "../data/hemoglobins.fasta",
              out_file = "out_Hb_psa.txt", out_format = "psa",
              ps_scan = "../ps_scan/ps_scan.pl", patterns_dat = "../data/prosite.dat", pf_scan = "../ps_scan/pfscan.exe")


# Running PS-Scan on Linux
runPsScan_LUM(in_file = in_file, out_file = out_file, out_format = out_format,
              ps_scan = ps_scan, patterns_dat = patterns_dat, pf_scan = pf_scan)

runPsScan_LUM(in_file = "../data/hemoglobins.fasta", out_file = out_file, out_format = out_format,
              ps_scan = "../ps_scan/ps_scan.pl", patterns_dat = "../prosite.dat", pf_scan ="../ps_scan/pfscan")

# Extend function for running PS-Scan
runPsScan_ext(in_file, out_format, out_file = "nopf.txt", ps_scan, patterns_dat, OS = "WIN") # OS = "Win" or "LUM" depends on
                                                                                        # operation system you're working on

runPsScan_ext(in_file = "../data/hemoglobins.fasta", out_format, out_file = "nopf.txt",
              ps_scan = "../ps_scan/ps_scan.pl", patterns_dat = "../prosite.dat", OS = "LUM")

runPsScan_ext(in_file = "data/hemoglobins.fasta", out_format = 'gff', out_file = "nopfGFF.txt",
              ps_scan = "ps_scan.pl", patterns_dat = "prosite.dat", OS = "WIN",pf_scan = pf_scan)

# For converting files to GFF format
motifs_psa <- "nopf.txt"
motifs_psa <- "data/out_Hb_psa.txt"


psaGFF <- read.psa(motifs_psa)
psaGFF <- read.psa("../PMScanR/out_Hb_psa.txt")
head(psaGFF)
nrow(psaGFF)
# Or read the GFF format
gff_format <- rtracklayer::import.gff("nopfGFF.txt")


gff_format <- rtracklayer::import.gff("data/out_Hb_gff.txt")
nrow(as.data.frame(gff_format))

head(gff_format)
head(as.data.frame(gff_format))

# Creating a matrix from GFF
rm(mom)
mom <- gff2matrix(psaGFF) # Where PSA was converted to GFF
debug(gff2matrix)
undebug(gff2matrix)
momGFF <- gff2matrix(as.data.frame(gff_format)) # Where the GFF format was read
mom <- gff2matrix(as.data.frame(psaGFF)) # Where the GFF format was read
mom <- gff2matrix(psaGFF)

head(momGFF)
write.csv2(mom, "MotifsOccurenceMatrix.csv")


# Creating a heatmap from matrix
hm1 <- matrix2hm(x = colnames(mom), # You can specify particular columns to show on heatmap
                       y = row.names(mom), # You can specify particular rows to show on heatmap
                       input = mom)
hm1

hmgff <- matrix2hm(x = colnames(momGFF), # You can specify particular columns to show on heatmap
                 y = row.names(momGFF), # You can specify particular rows to show on heatmap
                 input = momGFF)
hmgff

hm2 <- matrix2hm_2(x = colnames(mom), # You can specify particular columns to show on heatmap
                       y = row.names(mom), # You can specify particular rows to show on heatmap
                       input = mom)
hm2

hm2gff <- matrix2hm_2(x = colnames(momGFF), # You can specify particular columns to show on heatmap
                   y = row.names(momGFF)[1:10], # You can specify particular rows to show on heatmap
                   input = momGFF[c(1:10),])
hm2gff




# Making a seqlogo of selected region from raw sequences (FASTA file)

# Need to specify the starting and ending position
from = 10
to = 20

seq <- read.fasta(file = "../data/hemoglobins.fasta", seqtype = "AA")
seqShort <- extract_segments(seq = seq, from, to)
ggseqlogo(unlist(seqShort), seq_type= "aa")


# Making a seqlogo of selected identified motifs form PSA file
protein_motifs <- extract_protein_motifs(motifs_psa)
protein_motifs <- extract_protein_motifs(gff_format)

ggseqlogo(protein_motifs$PS60007, seq_type='aa')
ggseqlogo(protein_motifs[1], seq_type='aa')
ggseqlogo(protein_motifs[5], seq_type='aa')


# Making a PieChart from GFF file
piechartGFF <- freqPie(psaGFF)
piechartGFF

piechart <- freqPie(psaGFF)
piechart


# 1. ekstrakcja motywow do logo powinna byc z formatu GFF i wtedy wszystkie wczytane formaty daja nam mozliwosc zrobienia logo.
# 2. matrix2hm - 3x nazwa zmiennej!
# 3. podswietlanie wybranego motywu albo wybranej sekwencji
# 4. na heatmapie rownames i colnames
