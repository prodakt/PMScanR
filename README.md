# PMScanR
R library for the large-scale identification, analysis and visualization of protein motifs. 
The package integrates various methods to facilitate motif identification, characterization, and visualization. It includes functions for running PS-Scan, a PROSITE database tool. Additionally, PMScanR supports format conversion to GFF, enhancing downstream analyses such as graphical representation and database integration. The library offers multiple visualization tools, including heatmaps, sequence logos, and pie charts, enabling a deeper understanding of motif distribution and conservation. Through its integration with PROSITE, PMScanR provides access to up-to-date motif data.

There is a step-by-step tutorial available in the repository explaining, with examples, in turn the use/exploitation of each of the functions contained in this library. Tutorial available: PMScanR/inst/extdata/Tutorial_script and in this folder the "Step_by_Step.R" file is available for the detailed descripion of the usage of the individual functions.


## GUI
If the user prefers to perform the analysis using a graphical user interface (GUI), they can simply run the function runPMScanRShiny(). This will launch a Shiny app that opens an interactive window. The window can be used both within R and in a web browser, providing a clickable, user-friendly interface that allows the entire analysis, including visualizations, to be carried out without needing to write code.

## Command Line
Alternatively, if the user wishes to work directly with the code, the library provides a set of functions to perform the full analysis, including protein motif identification and visualization. This can be done through an R script, where users can execute and customize the analysis programmatically. Each function included in the package is described below, along with an explanation of its purpose and functionality.

### List of functions and their description:
-***runPsScan()***: A function that allows ps_scan to be run from user's operating system, they receive an output file containing information about the protein motifs - used for further analysis. The function works on the idea that the user selects the files needed to run ps_scan analysis, or if the user does not select these files they are automatically downloaded from the PROSITE database, then it is possible to specify the operating system the user is working on, but if this is not selected a message is displayed as to whether the detected operating system is the correct one (on a yes/no response basis).

-***readPM.prosite()***: A function that enables an input file in prosite format to be converted into a GFF format file, which is a more universal format used for analysis and visualisation such as heatmap or pie chart.

-***readPM.psa()***: A function that enables an input file in prosite format to be converted into a GFF format file, which is a more universal format used for analysis and visualisation such as heatmap or pie chart.

-***gff2matrix()***: Function used when the user receives a file in GFF format. this function enables the creation of a matrix of the occurrence from a GFF file of all the motifs in the entire sequence (or only those selected by the user), which contains 0/1 digits so even for very large-scale analyses it should not take up much disk space and is easy to visualise and analyse in any external program in which tables can be imported as calculation sheets.

-***matrix2hm()***: Once the matrix of the occurrence is obtained, this function is used to generate a heatmap, which is one of the options for visualising user data. This function provides the ability to adjust the plot size - this facilitates comparative analysis. By using the plotly library, the plots are interactive, allowing both extensive manipulation of the plot area, but also precise identification of individual results.

-***matrix2hm_2()***: Also the function used after receiving the matrix of the occurrence to generate the heatmap, but differing from the above heatmap dimension to allow easier identification of individual variations. By using the plotly library, the plots are interactive, allowing both extensive manipulation of the plot area, but also precise identification of individual results.

-***prepare_segments()***: Function used for another type of data visualisation which is a seqlogo generated based on the frequency of occurrence of individual amino acid residues in a selected part of the sequence that is, the consensus of the motif sequence. This function is used for the psa format input file.

-***extract_segments()***: Function used for another type of data visualisation which is a seqlogo generated based on the frequency of occurrence of individual amino acid residues in a selected part of the sequence, that is the defined region in selected sequences. Function used for a psa format input file, after using the prepare_segments() function. This function extracts the protein motifs present on the sequence thus preparing the file for seqlogo creation.

-***extract_protein_motifs()***: Function used for another type of data visualisation which is a seqlogo generated based on the frequency of occurrence of individual amino acid residues in a selected part of the sequence, that is the defined region in selected sequences. The function used for the input file in FASTA format, which extracts the protein motifs present on the sequence in the file, thus preparing them for data visualisation using seqlogo

There is also a visualization that generates a pie chart of the frequency of each motif type from the GFF format file containing information about the motif names and their locations. The pie chart shows the percentage of each motif/protein motif type in the analyzed dataset. This function uses the ggplot2 library. The pie chart allows a quick assessment of motif frequencies. If you want to run a pie chart visualization use the function named freqPie().

#### Examples of the use of the functions described above with the use of sample data contained in the extdata folder of the PMScanR repository:

```
Example of usage for running ps_scan for matif scanning by using runPsScan() function:

ps_scan <- "ps_scan/ps_scan.pl"        # Path to the PS-Scan perl script (e.g., 'ps_scan/ps_scan.pl')
patterns_dat <- "ps_scan/prosite.dat"   # Path to the PROSITE database file (e.g., 'ps_scan/prosite.dat')
out_format <- "psa"                    # Default output format for PS-Scan (PSA - PROSITE scan ASCII)
pf_scan <- "ps_scan/pfscan.exe"         # Path to the PFSCAN executable (Windows, e.g., 'ps_scan/pfscan.exe') - for Windows version
out_file <- "out_Hb_psa.txt"           # Default output filename for PSA format results (e.g., 'out_Hb_psa.txt')
in_file <- "../data/hemoglobins.fasta"  # Path to the input FASTA file (example: hemoglobin sequences, e.g., '../data/hemoglobins.fasta')

runPsScan(in_file = "protein.fasta", out_format = 'gff', out_file = "results_pfscan.gff",
          ps_scan = "ps_scan/ps_scan.pl", patterns_dat = "prosite.dat",
          pf_scan = "path/to/your/pfscan.exe", OS = "WIN")

# Example of usage for converting a PSA output file (using a filename variable):

motifs_psa <- "data/out_Hb_psa.txt"
psaGFF <- read.psa(motifs_psa)
head(psaGFF)  # Display the first few rows of the converted GFF-like data frame

# Example of usage for Creating matrix from directly read GFF format data:

momGFF <- gff2matrix(as.data.frame(gff_format))   # 'gff_format' is the GFF object imported by rtracklayer
head(momGFF) # Display the first few rows of the MOM from direct GFF input

# Example of usage for creating a heatmap using matrix2hm() - from PSA-converted Matrix:
# The 'matrix2hm()' function (assumed from PMScanR) generates a heatmap from a motif occurrence matrix. This example creates a heatmap from the MOM derived from PSA-converted GFF data ('mom').

hm1 <- matrix2hm(x = colnames(mom),   # 'x' argument specifies columns (motifs) to be shown on heatmap's x-axis
                 y = row.names(mom),    # 'y' argument specifies rows (sequences) to be shown on heatmap's y-axis
                 input = mom)           # 'input' argument takes the Motif Occurrence Matrix
hm1  Display the heatmap 'hm1'

# Example of usage for creating a heatmap using matrix2hm_2() - from PSA-converted Matrix:
# The 'matrix2hm_2()' function (assumed from PMScanR) is another function for heatmap generation, possibly with a different style or options compared to 'matrix2hm()'. This example uses 'matrix2hm_2()' to visualize the MOM from PSA-converted data ('mom').

hm2 <- matrix2hm_2(x = colnames(mom),
                   y = row.names(mom),
                   input = mom)
hm2 Display heatmap 'hm2'

# Exmaple of usage for creating sequence logo for a selected region from FASTA:

# Define the start and end position of the region you want to visualize as a sequence logo.
from_pos <- 10  # Starting position of the region
to_pos <- 20    # Ending position of the region

seq <- read.fasta(file = "../data/hemoglobins.fasta", seqtype = "AA")   # Read the FASTA file containing protein sequences
seqShort <- extract_segments(seq = seq, from_pos, to_pos)          # Extract segments from position 'from_pos' to 'to_pos' for all sequences
ggseqlogo(unlist(seqShort), seq_type= "aa")                        # Generate and display the sequence logo of the extracted segments

# This example below show how to use the function for running a user intarface GUI for a complete analysis without having to work with the code - Shiny app run:
# If you want you can use shiny to use all the features of the library with user friendly GUI helping to follow all the steps which are contained in this library
# To run Shiny app you can call function: runPMScanRShiny()
```
