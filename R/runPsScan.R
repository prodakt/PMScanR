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
#' if (interactive()) {
#'   in_file <- system.file("extdata", "hemoglobins.fasta", package = "PMScanR")
#'   out_file <- tempfile(fileext = ".gff")
#'   runPsScan(in_file = in_file, out_format = 'gff', out_file = out_file)
#' }

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

#'Download and Extract PS-Scan, pfscan, and patterns_dat Files
#'
#' Downloads `ps_scan.pl`, the appropriate `pf_scan` archive, and `prosite.dat`,
#' extracting the pfscan executable based on the operating system if not provided.
#' Checks for existing local files before downloading.
#'
#' @param os The operating system ("WIN", "LINUX", "MAC").
#' @param ps_scan Path to `ps_scan.pl` if already provided, otherwise `NULL`.
#' @param pf_scan Path to `pf_scan` executable if already provided, otherwise `NULL`.
#' @param patterns_dat Path to `patterns_dat` if already provided, otherwise `NULL`.
#' @return A list containing paths to `ps_scan`, `pf_scan` (extracted executable), and `patterns_dat`.
#' @importFrom utils download.file untar unzip
#' @noRd
download_files <- function(os, ps_scan = NULL, pf_scan = NULL, patterns_dat = NULL) {
  # Base URLs for downloading files
  base_url <- "https://ftp.expasy.org/databases/prosite/ps_scan/"
  data_url <- "https://ftp.expasy.org/databases/prosite/"
  
  # Configuration for files based on OS
  files_config <- list(
    WIN = list(
      ps_scan_name = "ps_scan.pl",
      pf_scan_archive_name = "ps_scan_win32.zip",
      pf_scan_exe_relative_path = "ps_scan/pfscan.exe", # Relative to extraction dir
      patterns_dat_name = "prosite.dat"
    ),
    LINUX = list(
      ps_scan_name = "ps_scan.pl",
      pf_scan_archive_name = "ps_scan_linux_x86_elf.tar.gz",
      pf_scan_exe_relative_path = "ps_scan/pfscan",    # Relative to extraction dir
      patterns_dat_name = "prosite.dat"
    ),
    MAC = list( 
      ps_scan_name = "ps_scan.pl",
      pf_scan_archive_name = "ps_scan_macosx.tar.gz",
      pf_scan_exe_relative_path = "ps_scan/pfscan",    # Relative to extraction dir
      patterns_dat_name = "prosite.dat"
    )
  )
  
  # Stop if OS is not supported for downloads
  if (!(os %in% names(files_config))) {
    stop("Unsupported operating system for file download.")
  }
  
  current_os_config <- files_config[[os]]
  extraction_dir <- "pfscan_extracted" # Directory for extracting pf_scan
  
  # --- Handle ps_scan.pl ---
  expected_ps_scan_local_path <- current_os_config$ps_scan_name # Assumed in current working dir
  
  if (is.null(ps_scan)) { # If user did not provide a path
    if (file.exists(expected_ps_scan_local_path)) {
      message(paste("Using existing local ps_scan.pl:", expected_ps_scan_local_path))
      ps_scan <- expected_ps_scan_local_path
    } else {
      ps_scan_url <- paste0(base_url, current_os_config$ps_scan_name)
      ps_scan_download_target <- current_os_config$ps_scan_name # Download to current dir
      message(paste("Attempting to download ps_scan.pl from:", ps_scan_url))
      tryCatch({
        download.file(ps_scan_url, ps_scan_download_target, mode = "wb")
        ps_scan <- ps_scan_download_target
        message(paste("Successfully downloaded ps_scan.pl to:", ps_scan))
      }, error = function(e) {
        warning(paste("Failed to download ps_scan.pl. Please download it manually from:", ps_scan_url, "\nError:", e$message))
        ps_scan <- NULL # Ensure ps_scan remains NULL if download fails
      })
    }
  } else { # User provided a path for ps_scan
    message(paste("Using provided ps_scan.pl path:", ps_scan))
    if (!file.exists(ps_scan)) {
      warning(paste("Provided ps_scan.pl path does not exist:", ps_scan))
      # The main runPsScan function will later stop if ps_scan is ultimately NULL or invalid
    }
  }
  
  # --- Handle pfscan executable ---
  # Expected local path for pf_scan executable (inside extraction_dir)
  expected_pf_scan_local_path <- file.path(extraction_dir, current_os_config$pf_scan_exe_relative_path)
  
  if (is.null(pf_scan)) { # If user did not provide a path
    if (os == "MAC") {
      message("For macOS, pf_scan is not downloaded by default. PS-Scan will use the --r option.")
      pf_scan <- NULL # Keep pf_scan NULL for MAC to use --r option
    } else { # For WIN and LINUX
      if (file.exists(expected_pf_scan_local_path)) {
        message(paste("Using existing local pfscan executable:", expected_pf_scan_local_path))
        pf_scan <- expected_pf_scan_local_path
        # Ensure executable on Linux if it exists
        if (os == "LINUX") {
          if (Sys.chmod(pf_scan, mode = "0755") == 0) {
            message(paste("Ensured existing pfscan is executable:", pf_scan))
          }
        }
      } else {
        pf_scan_archive_url <- paste0(base_url, current_os_config$pf_scan_archive_name)
        pf_scan_archive_download_target <- current_os_config$pf_scan_archive_name # Download archive to current dir
        message(paste("Attempting to download pfscan archive from:", pf_scan_archive_url))
        tryCatch({
          download.file(pf_scan_archive_url, pf_scan_archive_download_target, mode = "wb")
          message(paste("Successfully downloaded pfscan archive:", pf_scan_archive_download_target))
          
          # Ensure extraction directory exists
          if (!dir.exists(extraction_dir)) {
            dir.create(extraction_dir, recursive = TRUE)
          }
          
          # Extract the archive
          if (os == "WIN") {
            unzip(pf_scan_archive_download_target, exdir = extraction_dir)
          } else if (os == "LINUX") { # Also applies to other tar.gz if any were added
            untar(pf_scan_archive_download_target, exdir = extraction_dir)
          }
          
          # Assign the expected path after extraction attempt
          pf_scan <- expected_pf_scan_local_path
          
          if (!file.exists(pf_scan)) {
            stop(paste("Failed to find pfscan executable at", pf_scan, "after attempting extraction from", pf_scan_archive_download_target, ". Check archive integrity and extraction process."))
          }
          message(paste("Successfully extracted pfscan to:", pf_scan))
          
          # Make executable on Linux
          if (os == "LINUX") {
            if (Sys.chmod(pf_scan, mode = "0755") == 0) {
              message(paste("Made downloaded pfscan executable:", pf_scan))
            }
          }
          
          # Optional: Clean up downloaded archive after successful extraction
          # if (file.exists(pf_scan_archive_download_target)) {
          #   file.remove(pf_scan_archive_download_target)
          #   message(paste("Cleaned up downloaded archive:", pf_scan_archive_download_target))
          # }
          
        }, error = function(e) {
          warning(paste("Failed to download or extract pfscan. Please download/extract it manually from:", pf_scan_archive_url, "\nError:", e$message))
          pf_scan <- NULL # Ensure pf_scan remains NULL if download/extraction fails
        })
      }
    }
  } else { # User provided a path for pf_scan
    message(paste("Using provided pfscan path:", pf_scan))
    if (!file.exists(pf_scan)) {
      warning(paste("Provided pfscan path does not exist:", pf_scan))
    } else {
      # If file exists and is on Linux, ensure it's executable
      if (os == "LINUX") {
        if(Sys.chmod(pf_scan, mode = "0755") == 0) {
          message(paste("Ensured provided pfscan is executable:", pf_scan))
        } else {
          warning(paste("Failed to make provided pfscan executable:", pf_scan))
        }
      }
    }
  }
  
  # --- Handle prosite.dat (patterns_dat) ---
  expected_patterns_dat_local_path <- current_os_config$patterns_dat_name # Assumed in current working dir
  
  if (is.null(patterns_dat)) { # If user did not provide a path
    if (file.exists(expected_patterns_dat_local_path)) {
      message(paste("Using existing local prosite.dat:", expected_patterns_dat_local_path))
      patterns_dat <- expected_patterns_dat_local_path
    } else {
      patterns_dat_url <- paste0(data_url, current_os_config$patterns_dat_name)
      patterns_dat_download_target <- current_os_config$patterns_dat_name # Download to current dir
      message(paste("Attempting to download prosite.dat from:", patterns_dat_url))
      tryCatch({
        download.file(patterns_dat_url, patterns_dat_download_target, mode = "wb")
        patterns_dat <- patterns_dat_download_target
        message(paste("Successfully downloaded prosite.dat to:", patterns_dat))
      }, error = function(e) {
        warning(paste("Failed to download prosite.dat. Please download it manually from:", patterns_dat_url, "\nError:", e$message))
        patterns_dat <- NULL # Ensure patterns_dat remains NULL if download fails
      })
    }
  } else { # User provided a path for patterns_dat
    message(paste("Using provided prosite.dat path:", patterns_dat))
    if (!file.exists(patterns_dat)) {
      warning(paste("Provided prosite.dat path does not exist:", patterns_dat))
    }
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
#' Helper function to execute an external command based on the operating system.
#' It now checks the command's exit status before printing a success message.
#'
#' @param command The command string to execute.
#' @param os The operating system ("WIN", "LINUX", "MAC").
#' @param out_file file path where the analysis result is expected to be saved.
#'        This is used for the success message.
#' @return Invisibly returns the exit status of the command (0 for success,
#'         non-zero for errors, or specific codes depending on the command).
#' @noRd
#' @keywords internal system command execution
#'
execute_command <- function(command, os, out_file) {
  # Print a message indicating the start of the analysis
  cat("Beginning of Prosite analysis", "\n")
  
  # Initialize status_code to a non-success value
  status_code <- -1
  
  # Execute the command based on the operating system
  if (os == "LINUX" || os == "MAC") {
    # On Linux or Mac, use system(); it returns an OS-dependent exit status.
    status_code <- system(command)
  } else if (os == "WIN") {
    # On Windows, use shell(); it also returns an exit status.
    status_code <- shell(command)
  } else {
    # If the OS is not supported, stop execution with an error message.
    stop(paste("Unsupported operating system specified:", os))
  }
  
  # Check the command's exit status to determine if it was successful
  if (status_code == 0) {
    # If status is 0, print a success message
    message("Prosite analysis done and saved to ", out_file)
  } else {
    # If status is not 0, print a warning indicating a potential failure
    warning(paste("Prosite analysis may have failed or encountered an error.",
                  "Command exit status:", status_code,
                  "\nPlease check the console for any error messages from the command itself."))
  }
  
  # Return the status code invisibly.
  invisible(status_code)
}
