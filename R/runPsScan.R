#' Run PS-Scan
#'
#' Extended function to run PS-Scan with automatic OS detection, file downloading, extraction, and improved error handling.
#'
#' @param in_file Input file containing protein sequences.
#' @param out_file A character string specifying the path to the output file where results will be saved.
#' @param out_format Specifying the output format (e.g., "fasta").
#' @param ps_scan A character string specifying the path to the PS-Scan Perl script. If NULL, it will be downloaded.
#' @param patterns_dat A character string specifying the path to the PROSITE patterns database file. If NULL, it will be downloaded.
#'                     This file can be downloaded from: https://ftp.expasy.org/databases/prosite/
#' @param pf_scan A character string specifying the path to the pfscan executable. If NULL, it will be downloaded and extracted based on OS (except for MAC).
#' @param OS Operating system ("WIN", "LINUX", "MAC"). If NULL, it will be detected automatically.
#' @return Writes the results of the PS-Scan analysis to the specified output file.
#' @examples
#' {
#' ps_scan <- "path/to/ps_scan.pl"
#' patterns_dat <- "path/to/prosite.dat"
#' out_format <- "fasta"
#' pf_scan <- "path/to/pfscan.exe"
#' out_file <- "out_Hb_fasta.txt"
#' in_file <- "hemoglobins.fasta"
#' runPsScan(in_file, out_file, out_format, ps_scan, patterns_dat, pf_scan, OS = "WIN")
#' }
#' @export
runPsScan <- function(in_file, out_file, out_format, ps_scan = NULL, patterns_dat = NULL, pf_scan = NULL, OS = NULL) {
  # Detect and confirm OS if not provided
  if (is.null(OS)) {
    detected_os <- detect_os()
    OS <- confirm_os(detected_os)
  }

  # Download and extract files if not provided
  downloaded_files <- download_files(OS, ps_scan, pf_scan, patterns_dat)

  # Update paths with downloaded/extracted files if necessary
  ps_scan <- if (is.null(ps_scan)) downloaded_files$ps_scan else ps_scan
  pf_scan <- if (is.null(pf_scan)) downloaded_files$pf_scan else pf_scan
  patterns_dat <- if (is.null(patterns_dat)) downloaded_files$patterns_dat else patterns_dat

  # Verify required files are available
  if (is.null(ps_scan)) {
    stop("ps_scan is not provided and could not be downloaded. Please specify the path manually.")
  }
  if (is.null(patterns_dat)) {
    stop("patterns_dat is not provided and could not be downloaded. Please specify the path manually.")
  }

  # For MAC, allow pf_scan to be NULL to use --r option; for other OSes, stop if NULL after download attempt
  if (is.null(pf_scan) && OS != "MAC") {
    stop("pf_scan is not provided and could not be downloaded/extracted. Please specify the path manually.")
  }

  # Inform user when running on MAC without pf_scan
  if (OS == "MAC" && is.null(pf_scan)) {
    message("On macOS, running PS-Scan without pf_scan executable using the --r option. This may have limitations.")
  }

  # Construct and execute the command
  command <- construct_command(ps_scan, patterns_dat, in_file, out_format, pf_scan, out_file)
  execute_command(command, OS, out_file)
}

#' Detect Operating System
#'
#' Automatically detects the operating system using \code{Sys.info()}.
#'
#' @return A character string indicating the OS: "WIN" for Windows, "LINUX" for Linux, "MAC" for macOS, or "UNSUPPORTED" for other systems.
#' @examples
#' detect_os()
#' @noRd
detect_os <- function() {
  sys_info <- Sys.info()
  os <- sys_info["sysname"]
  if (os == "Windows") {
    return("WIN")
  } else if (os == "Linux") {
    return("LINUX")
  } else if (os == "Darwin") {  # macOS
    return("MAC")
  } else {
    return("UNSUPPORTED")
  }
}

#' Confirm Operating System
#'
#' Prompts the user to confirm the detected operating system via console input.
#'
#' @param detected_os The detected operating system from \code{detect_os()}.
#' @return The confirmed OS if the user selects "1" (yes), otherwise stops with an error.
#' @examples
#' {
#' confirm_os("WIN")
#' }
#' @noRd
confirm_os <- function(detected_os) {
  cat("Detected operating system:", detected_os, "\n")
  confirmation <- readline("Is this correct? (1 for yes, 2 for no): ")
  if (confirmation == "1") {
    return(detected_os)
  } else {
    stop("Operating system detection failed. Please specify the OS manually using the 'OS' parameter.")
  }
}

#' Download and Extract PS-Scan, pfscan, and patterns_dat Files
#'
#' Downloads \code{ps_scan.pl}, the appropriate \code{pf_scan} archive, and \code{prosite.dat}, extracting the pfscan executable based on the operating system if not provided.
#'
#' @param os The operating system ("WIN", "LINUX", "MAC").
#' @param ps_scan Path to \code{ps_scan.pl} if already provided, otherwise \code{NULL}.
#' @param pf_scan Path to \code{pf_scan} executable if already provided, otherwise \code{NULL}.
#' @param patterns_dat Path to \code{patterns_dat} if already provided, otherwise \code{NULL}.
#' @return A list containing paths to \code{ps_scan}, \code{pf_scan} (extracted executable), and \code{patterns_dat}.
#' @examples
#' {
#' download_files("WIN", ps_scan = NULL, pf_scan = NULL, patterns_dat = NULL)
#' }
#' @importFrom utils download.file untar unzip
#' @noRd
download_files <- function(os, ps_scan = NULL, pf_scan = NULL, patterns_dat = NULL) {
  base_url <- "https://ftp.expasy.org/databases/prosite/ps_scan/"
  data_url <- "https://ftp.expasy.org/databases/prosite/"
  files <- list(
    WIN = list(
      ps_scan = "ps_scan.pl",
      pf_scan_archive = "ps_scan_win32.zip",
      pf_scan_exe = "ps_scan/pfscan.exe",  # Corrected path after extraction
      patterns_dat = "prosite.dat"
    ),
    LINUX = list(
      ps_scan = "ps_scan.pl",
      pf_scan_archive = "ps_scan_linux_x86_elf.tar.gz",
      pf_scan_exe = "pfscan",              # Direct executable name
      patterns_dat = "prosite.dat"
    ),
    MAC = list(
      ps_scan = "ps_scan.pl",
      pf_scan_archive = "ps_scan_macosx.tar.gz",
      pf_scan_exe = "pfscan",              # Direct executable name
      patterns_dat = "prosite.dat"
    )
  )

  if (!(os %in% names(files))) {
    stop("Unsupported operating system for file download.")
  }

  # Download ps_scan if not provided
  if (is.null(ps_scan)) {
    ps_scan_url <- paste0(base_url, files[[os]]$ps_scan)
    ps_scan_file <- basename(ps_scan_url)
    tryCatch({
      download.file(ps_scan_url, ps_scan_file, mode = "wb")
      ps_scan <- ps_scan_file
    }, error = function(e) {
      warning(paste("Failed to download ps_scan.pl. Please download it manually from:", ps_scan_url))
      ps_scan <- NULL
    })
  }

  # Download and extract pf_scan if not provided, but skip downloading for MAC
  if (is.null(pf_scan)) {
    if (os != "MAC") {
      pf_scan_url <- paste0(base_url, files[[os]]$pf_scan_archive)
      pf_scan_archive <- basename(pf_scan_url)
      tryCatch({
        download.file(pf_scan_url, pf_scan_archive, mode = "wb")

        # Extract the archive based on OS
        if (os == "WIN") {
          unzip(pf_scan_archive, exdir = "pfscan_temp")
          pf_scan <- file.path("pfscan_temp", files[[os]]$pf_scan_exe)
        } else if (os == "LINUX") {
          untar(pf_scan_archive, exdir = "pfscan_temp")
          pf_scan <- file.path("pfscan_temp", files[[os]]$pf_scan_exe)
        }

        # Verify extraction
        if (!file.exists(pf_scan)) {
          stop("Failed to extract pf_scan executable from archive.")
        }

        # Make executable on Linux
        if (os == "LINUX") {
          Sys.chmod(pf_scan, mode = "0755")
        }
      }, error = function(e) {
        warning(paste("Failed to download or extract pf_scan. Please download it manually from:", pf_scan_url))
        pf_scan <- NULL
      })
    } else {
      # For MAC, do not download pf_scan; keep it NULL to use --r option
      pf_scan <- NULL
      # Old behavior for MAC (commented out for future reference):
      # pf_scan_url <- paste0(base_url, files[[os]]$pf_scan_archive)
      # pf_scan_archive <- basename(pf_scan_url)
      # tryCatch({
      #   download.file(pf_scan_url, pf_scan_archive, mode = "wb")
      #   untar(pf_scan_archive, exdir = "pfscan_temp")
      #   pf_scan <- file.path("pfscan_temp", files[[os]]$pf_scan_exe)
      #   if (!file.exists(pf_scan)) {
      #     stop("Failed to extract pf_scan executable from archive.")
      #   }
      #   Sys.chmod(pf_scan, mode = "0755")
      # }, error = function(e) {
      #   warning(paste("Failed to download or extract pf_scan. Please download it manually from:", pf_scan_url))
      #   pf_scan <- NULL
      # })
    }
  }

  # Download patterns_dat if not provided
  if (is.null(patterns_dat)) {
    patterns_dat_url <- paste0(data_url, files[[os]]$patterns_dat)
    patterns_dat_file <- basename(patterns_dat_url)
    tryCatch({
      download.file(patterns_dat_url, patterns_dat_file, mode = "wb")
      patterns_dat <- patterns_dat_file
    }, error = function(e) {
      warning(paste("Failed to download prosite.dat. Please download it manually from:", patterns_dat_url))
      patterns_dat <- NULL
    })
  }

  return(list(ps_scan = ps_scan, pf_scan = pf_scan, patterns_dat = patterns_dat))
}

#' Construct PS-Scan Command
#'
#' Helper function to build the command string for running PS-Scan.
#'
#' @param ps_scan Path to the PS-Scan Perl script.
#' @param patterns_dat Path to the PROSITE patterns database file.
#' @param in_file Input file containing protein sequences.
#' @param out_format Output format (e.g., "fasta").
#' @param pf_scan Path to the pfscan executable, or NULL to use the --r option.
#' @param out_file Path to the output file.
#' @return A character string representing the PS-Scan command.
#' @noRd
construct_command <- function(ps_scan, patterns_dat, in_file, out_format, pf_scan, out_file) {
  pf_scan_option <- if (is.null(pf_scan)) "--r" else paste0("--pfscan ", pf_scan)
  paste0("perl ", ps_scan,
         " -d ", patterns_dat,
         " ", in_file,
         " -o ", out_format,
         " ", pf_scan_option,
         " >> ", out_file)
}

#' Execute System Command
#'
#' Helper function to execute the PS-Scan command based on the operating system.
#'
#' @param command The command string to execute.
#' @param os The operating system ("WIN", "LINUX", "MAC").
#' @param out_file file path where analysis result is saved
#' @noRd
execute_command <- function(command, os, out_file) {
  cat("Beggining of Prosite analysis", "\n")
  if (os == "LINUX" || os == "MAC") {
    system(command)
  } else if (os == "WIN") {
    shell(command)
  } else {
    stop("Unsupported operating system")
  }
  message("Prosite analysis done and saved to ", out_file)
}

