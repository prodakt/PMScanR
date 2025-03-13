#' Run PS-Scan on Linux
#' 
#' This function runs the PS-Scan tool on a Linux system to scan protein sequences for PROSITE patterns.
#' This function constructs a command to execute PS-Scan and runs it using the Linux.
#' 
#' The arguments in this function are assumed in advance, where the 
#' ps_scan folder is available for download: https://ftp.expasy.org/databases/prosite/ 
#' and after unzipping, this is its target name.
#'
#' @param in_file Input file containing a protein sequences.
#' @param out_file A character string specifying the path to the output file where results will be saved.
#' @param out_format Specifying the output format (e.g. "fasta").
#' @param ps_scan A character string specifying the path to the PS-Scan Perl script.
#' @param patterns_dat A character string specifying the path to the PROSITE patterns database file
#'                      This file can be downloaded from: https://ftp.expasy.org/databases/prosite/
#' @param pf_scan A character string specifying additional parameters for the PS-Scan tool (e.g., options for pfscan).
#' @return It writes the results of the PS-Scan analysis to the specified output file.
#' @example 
#' ps_scan <- "path to file/ps_scan.pl"
#' patterns_dat <- "path to file/prosite.dat"
#' out_format <- "fasta"
#' pf_scan <- "path to file/pfscan.exe"
#' out_file <- "out_Hb_fasta.txt"
#' in_file <- "hemoglobins.fasta"
#' runPsScan_LUM(in_file = in_file, out_file = out_file, out_format = out_format, ps_scan = ps_scan, 
#'                patterns_dat = patterns_dat, pf_scan = pf_scan)
#'                
#' @export

runPsScan_LUM <- function(in_file, out_file = "prosite_psa.out", 
                          out_format = "psa", ps_scan = "ps_scan/ps_scan.pl", 
                          patterns_dat = "prosite.dat", pf_scan = "ps_scan/pfscan"){
  comScan <- paste0("perl ", ps_scan,
                    " -d ", patterns_dat,
                    " ", in_file,
                    " -o ", out_format,
                    " --pfscan ", pf_scan,
                    " >> ", out_file
  )
  system(comScan)
}
