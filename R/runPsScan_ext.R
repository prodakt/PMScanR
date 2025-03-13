#' Run PS-Scan
#' 
#' Extended function of runPsScan_Win and runPsScan_LUM with 
#' more arguments to handle the function with the possibility to manipulate the arguments 
#'
#'
#' @param in_file Input file containing a protein sequences.
#' @param out_file A character string specifying the path to the output file where results will be saved.
#' @param out_format Specifying the output format (e.g. "fasta").
#' @param ps_scan A character string specifying the path to the PS-Scan Perl script.
#' @param patterns_dat A character string specifying the path to the PROSITE patterns database file
#'                      This file can be downloaded from: https://ftp.expasy.org/databases/prosite/
#' @param pf_scan A character string specifying additional parameters for the PS-Scan tool (e.g., options for pfscan).
#' @param OS Operation system you're working on.
#' @return It writes the results of the PS-Scan analysis to the specified output file.
#' @example 
#' ps_scan <- "path to file/ps_scan.pl"
#' patterns_dat <- "path to file/prosite.dat"
#' out_format <- "fasta"
#' pf_scan <- "path to file/pfscan.exe"
#' out_file <- "out_Hb_fasta.txt"
#' in_file <- "hemoglobins.fasta"
#' runPsScan_ext(in_file, out_file, out_format, ps_scan, patterns_dat, pf_scan, OS = "WIN")
#' 
#' @export


runPsScan_ext <- function(in_file, out_file, out_format, ps_scan, patterns_dat, pf_scan = NULL, OS = "WIN"){
  comScan <- paste0("perl ", ps_scan,
                    " -d ", patterns_dat,
                    " ", in_file,
                    " -o ", out_format,
                    if(is.null(pf_scan)) {
                      paste0(" --r ")
                    } else {
                      paste0(" --pfscan ", pf_scan)
                    },
                    " >> ", out_file
  )
  if(OS == "LUM"){
    system(comScan)
  }else if(OS == "WIN") {
    shell(comScan)
  }else{
    Paste0("Unsupported operating system")
  }
  
}

