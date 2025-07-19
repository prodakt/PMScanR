#' Run PS-Scan with Caching and Improved Execution
#'
#' This function runs the PROSITE ps_scan tool. It handles the downloading and
#' caching of required executables and databases using BiocFileCache, detects
#' the operating system, and executes the scan in a robust manner.
#'
<<<<<<< Updated upstream
#' @param in_file Input file containing protein sequences.
#' @param out_file A character string specifying the path to the output file where results will be saved.
#' @param out_format Specifying the output format (e.g., "fasta").
#' @param ps_scan A character string specifying the path to the PS-Scan Perl script. If NULL, it will be downloaded.
#' @param patterns_dat A character string specifying the path to the PROSITE patterns database file. If NULL, it will be downloaded.
#'                     This file can be downloaded from: https://ftp.expasy.org/databases/prosite/
#' @param pf_scan A character string specifying the path to the pfscan executable. If NULL, it will be downloaded and extracted based on OS.
#' @param OS Operating system ("WIN", "LUM", "MAC"). If NULL, it will be detected automatically.
#' @return Writes the results of the PS-Scan analysis to the specified output file.
#' @examples 
#' \dontrun{
#' ps_scan <- "path/to/ps_scan.pl"
#' patterns_dat <- "path/to/prosite.dat"
#' out_format <- "fasta"
#' pf_scan <- "path/to/pfscan.exe"
#' out_file <- "out_Hb_fasta.txt"
#' in_file <- "hemoglobins.fasta"
#' runPsScan(in_file, out_file, out_format, ps_scan, patterns_dat, pf_scan, OS = "WIN")
#' }
=======
#' @param in_file Path to the input file containing protein sequences.
#' @param out_file Path for the output file where results will be saved.
#' @param out_format The output format for ps_scan (e.g., 'gff', 'psa').
#' @param os The operating system ('WIN', 'LINUX', 'MAC'). If NULL, it is
#'   detected automatically.
#'
#' @return Invisibly returns the exit status of the ps_scan command. The primary
#'   output is the result file created at `out_file`.
#' @examples
#' # This example is resource-intensive and requires an internet connection
#' # on first run to cache necessary files.
#' if (interactive()) {
#'   # Create a dummy input file for the example
#'   fasta_content <- c(">sp|P02025|HEMA_MESAU Hemoglobin subunit alpha",
#'                      "MVLSAADKGNVKAAWGKVGGHAAEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHG",
#'                      "KKVADALTNAVAHVDDMPNALSALSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPAEFT",
#'                      "PAVHASLDKFLASVSTVLTSKYR")
#'   in_file <- tempfile(fileext = ".fasta")
#'   writeLines(fasta_content, in_file)
#'
#'   out_file <- tempfile(fileext = ".gff")
#'
#'   # The first run will download and cache ~100MB of data.
#'   # Subsequent runs will use the cached files.
#'   runPsScan(in_file = in_file, out_format = 'gff', out_file = out_file)
#'
#'   # Clean up temporary files
#'   unlink(in_file)
#'   unlink(out_file)
#' }
#'
#' @importFrom BiocFileCache BiocFileCache bfcrpath bfcadd bfcquery
#' @importFrom utils untar unzip
>>>>>>> Stashed changes
#' @export
runPsScan <- function(in_file, out_file, out_format, os = NULL) {
  # 1. Detect OS if not provided
  if (is.null(os)) {
    os <- detectOs()
  }
<<<<<<< Updated upstream
  
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
  if (is.null(pf_scan)) {
    stop("pf_scan is not provided and could not be downloaded/extracted. Please specify the path manually.")
  }
  if (is.null(patterns_dat)) {
    stop("patterns_dat is not provided and could not be downloaded. Please specify the path manually.")
  }
  
  # Construct and execute the command
  command <- construct_command(ps_scan, patterns_dat, in_file, out_format, pf_scan, out_file)
  execute_command(command, OS)
}

#' Detect Operating System
#'
#' Automatically detects the operating system using \code{Sys.info()}.
#'
#' @return A character string indicating the OS: "WIN" for Windows, "LUM" for Linux, "MAC" for macOS, or "UNSUPPORTED" for other systems.
#' @examples
#' detect_os()
#' @export
detect_os <- function() {
  sys_info <- Sys.info()
  os <- sys_info["sysname"]
  if (os == "Windows") {
    return("WIN")
  } else if (os == "Linux") {
    return("LUM")
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
#' \dontrun{
#' confirm_os("WIN")
#' }
#' @export
confirm_os <- function(detected_os) {
  cat("Detected operating system:", detected_os, "\n")
  confirmation <- readline("Is this correct? (1 for yes, 2 for no): ")
  if (confirmation == "1") {
    return(detected_os)
  } else {
    stop("Operating system detection failed. Please specify the OS manually using the 'OS' parameter.")
=======

  # 2. Get paths to required files, downloading and caching them if needed
  # This function now uses BiocFileCache
  ps_scan_path <- getCachedPsScanTool("ps_scan.pl", os)
  patterns_dat_path <- getCachedPsScanTool("prosite.dat", os)

  # pfscan is optional for MAC, required for others
  pf_scan_path <- NULL
  if (os != "MAC") {
    pf_scan_path <- getCachedPsScanTool("pfscan", os)
>>>>>>> Stashed changes
  }

<<<<<<< Updated upstream
#' Download and Extract PS-Scan, pfscan, and patterns_dat Files
#'
#' Downloads \code{ps_scan.pl}, the appropriate \code{pf_scan} archive, and \code{prosite.dat}, extracting the pfscan executable based on the operating system if not provided.
#'
#' @param os The operating system ("WIN", "LUM", "MAC").
#' @param ps_scan Path to \code{ps_scan.pl} if already provided, otherwise \code{NULL}.
#' @param pf_scan Path to \code{pf_scan} executable if already provided, otherwise \code{NULL}.
#' @param patterns_dat Path to \code{patterns_dat} if already provided, otherwise \code{NULL}.
#' @return A list containing paths to \code{ps_scan}, \code{pf_scan} (extracted executable), and \code{patterns_dat}.
#' @examples
#' \dontrun{
#' download_files("WIN", ps_scan = NULL, pf_scan = NULL, patterns_dat = NULL)
#' }
#' @export
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
    LUM = list(
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
  
  # Download and extract pf_scan if not provided
  if (is.null(pf_scan)) {
    pf_scan_url <- paste0(base_url, files[[os]]$pf_scan_archive)
    pf_scan_archive <- basename(pf_scan_url)
    tryCatch({
      download.file(pf_scan_url, pf_scan_archive, mode = "wb")
      
      # Extract the archive based on OS
      if (os == "WIN") {
        unzip(pf_scan_archive, exdir = "pfscan_temp")
        pf_scan <- file.path("pfscan_temp", files[[os]]$pf_scan_exe)
      } else if (os %in% c("LUM", "MAC")) {
        untar(pf_scan_archive, exdir = "pfscan_temp")
        pf_scan <- file.path("pfscan_temp", files[[os]]$pf_scan_exe)
      }
      
      # Verify extraction
      if (!file.exists(pf_scan)) {
        stop("Failed to extract pf_scan executable from archive.")
      }
      
      # Make executable on Linux/macOS
      if (os %in% c("LUM", "MAC")) {
        Sys.chmod(pf_scan, mode = "0755")
      }
    }, error = function(e) {
      warning(paste("Failed to download or extract pf_scan. Please download it manually from:", pf_scan_url))
      pf_scan <- NULL
    })
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
=======
  # 3. Construct the command for system2
  cmd_args <- constructCommand(
    ps_scan_script = ps_scan_path,
    patterns_db = patterns_dat_path,
    input_fasta = in_file,
    output_format = out_format,
    pfscan_exec = pf_scan_path,
    output_file = out_file
  )

  # 4. Execute the command
  executeCommand(args = cmd_args, os = os, out_file = out_file)
>>>>>>> Stashed changes
}

#' Get a Cached PS-Scan Tool/Database File
#'
<<<<<<< Updated upstream
#' Helper function to build the command string for running PS-Scan.
#'
#' @param ps_scan Path to the PS-Scan Perl script.
#' @param patterns_dat Path to the PROSITE patterns database file.
#' @param in_file Input file containing protein sequences.
#' @param out_format Output format (e.g., "fasta").
#' @param pf_scan Path to the pfscan executable, or NULL to use the --r option.
#' @param out_file Path to the output file.
#' @return A character string representing the PS-Scan command.
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
#' @param os The operating system ("WIN", "LUM", "MAC").
execute_command <- function(command, os) {
  cat("Beggining of Prosite analysis")
  if (os == "LUM" || os == "MAC") {
    system(command)
  } else if (os == "WIN") {
    shell(command)
  } else {
    stop("Unsupported operating system")
  }
  cat("Prosite analysis done and saved to ", out_file)
=======
#' Uses BiocFileCache to manage downloading and storing ps_scan files.
#' @param tool_name The specific tool to get: "ps_scan.pl", "prosite.dat", or "pfscan".
#' @param os The operating system ("WIN", "LINUX", "MAC").
#' @return The local path to the cached file.
#' @noRd
getCachedPsScanTool <- function(tool_name, os) {
  cache <- BiocFileCache::BiocFileCache(cache = "PMScanR_cache", ask = FALSE)

  base_url <- "https://ftp.expasy.org/databases/prosite/"

  resource_map <- list(
    `ps_scan.pl` = paste0(base_url, "ps_scan/ps_scan.pl"),
    `prosite.dat` = paste0(base_url, "prosite.dat"),
    `pfscan_WIN` = paste0(base_url, "ps_scan/ps_scan_win32.zip"),
    `pfscan_LINUX` = paste0(base_url, "ps_scan/ps_scan_linux_x86_elf.tar.gz"),
    `pfscan_MAC` = paste0(base_url, "ps_scan/ps_scan_macosx.tar.gz")
  )

  query_name <- if (tool_name == "pfscan") paste0(tool_name, "_", os) else tool_name

  cache_info <- BiocFileCache::bfcquery(cache, query = query_name, field = "rname")

  if (nrow(cache_info) == 0) {
    # File not in cache, download it
    message(sprintf("'%s' not found in cache. Downloading...", query_name))
    download_url <- resource_map[[query_name]]
    if (is.null(download_url)) {
      stop(sprintf("No download URL defined for '%s'", query_name))
    }
    fpath <- BiocFileCache::bfcadd(cache, rname = query_name, fpath = download_url)
    message("Download complete.")
  } else {
    # File is in cache, get its path
    fpath <- cache_info$rpath[1]
  }

  # Handle extraction for compressed pfscan archives
  if (tool_name == "pfscan") {
    exec_path_map <- list(
      WIN = "ps_scan/pfscan.exe",
      LINUX = "ps_scan/pfscan",
      MAC = "ps_scan/pfscan"
    )

    # The cache path for the pfscan executable itself
    exec_rname <- paste0("executable_", query_name)
    exec_cache_info <- BiocFileCache::bfcquery(cache, exec_rname)

    if(nrow(exec_cache_info) == 0) {
      message(sprintf("Extracting executable from '%s'...", basename(fpath)))
      exdir <- tempfile()
      dir.create(exdir)

      if (os == "WIN") {
        utils::unzip(fpath, exdir = exdir)
      } else {
        utils::untar(fpath, exdir = exdir)
      }

      # The full path to the extracted executable
      extracted_file_path <- file.path(exdir, exec_path_map[[os]])
      if(!file.exists(extracted_file_path)) stop("Could not find executable after extraction.")

      # Add the extracted executable to the cache for future use
      final_exec_path <- BiocFileCache::bfcadd(cache, rname = exec_rname, fpath = extracted_file_path)

      # Make it executable on Linux/Mac
      if(os != "WIN") Sys.chmod(final_exec_path, mode = "0755")

      return(final_exec_path)
    } else {
      # Return path to already cached executable
      final_exec_path <- exec_cache_info$rpath[1]
      if(os != "WIN" && file.access(final_exec_path, 1) != 0) {
        Sys.chmod(final_exec_path, mode = "0755") # Ensure executable
      }
      return(final_exec_path)
    }
  }

  # For non-archived files, just return the path
  return(fpath)
}


#' Construct Command Arguments for system2
#' @return A character vector of arguments for `system2`.
#' @noRd
constructCommand <- function(ps_scan_script, patterns_db, input_fasta,
                             output_format, pfscan_exec, output_file) {
  # Base arguments for the perl script
  args <- c(
    ps_scan_script,
    "-d", patterns_db,
    input_fasta,
    "-o", output_format
  )

  # Add pfscan executable path or the --r option
  if (is.null(pfscan_exec)) {
    args <- c(args, "--r")
  } else {
    args <- c(args, "--pfscan", pfscan_exec)
  }

  return(args)
}


#' Execute the PS-Scan Command using system2
#' @param args A character vector of command arguments.
#' @param os The operating system.
#' @param out_file Path to the output file (for messages).
#' @return Invisibly returns the command's exit status.
#' @noRd
executeCommand <- function(args, os, out_file) {
  message("Starting PROSITE analysis...")
  command <- "perl"
  status_code <- system2(
    command,
    args = args,
    stdout = out_file,
    stderr = ""
  )

  if (status_code == 0) {
    message(sprintf("PROSITE analysis finished successfully. Results saved to: %s", out_file))
  } else {
    warning(sprintf("PROSITE analysis may have failed with exit code %d.", status_code))
    warning("Check that Perl is installed and in your system's PATH.")
  }

  invisible(status_code)
>>>>>>> Stashed changes
}

