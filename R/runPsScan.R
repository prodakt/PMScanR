#' Run PS-Scan with Flexible Configuration
#'
#' This function runs the PROSITE ps_scan tool. It allows users to provide their
#' own paths to required files (database, script, executable). If paths are not
#' provided, it handles the downloading and caching of required executables and
#' databases using BiocFileCache.
#'
#' @param in_file Path to the input file containing protein sequences.
#' @param out_file Path for the output file where results will be saved.
#' @param out_format The output format for ps_scan (e.g., 'gff', 'psa', 'scan').
#' @param database_path (Optional) Path to the PROSITE database file (`prosite.dat`).
#'   If NULL, the file is retrieved from the internal cache.
#' @param ps_scan_path (Optional) Path to the `ps_scan.pl` script.
#'   If NULL, the file is retrieved from the internal cache.
#' @param pfscan_path (Optional) Path to the `pfscan` executable (or `pfscan.exe`).
#'   If NULL, the file is retrieved from the internal cache (except on Mac, where it is optional).
#' @param os The operating system ('WIN', 'LINUX', 'MAC'). If NULL, it is
#'   detected automatically.
#'
#' @return Invisibly returns the exit status of the ps_scan command. The primary
#'   output is the result file created at `out_file`.
#'
#' @importFrom BiocFileCache BiocFileCache bfcrpath bfcadd bfcquery
#' @importFrom utils untar unzip
#' @export
runPsScan <- function(in_file,
                      out_file,
                      out_format = "scan",
                      database_path = NULL,
                      ps_scan_path = NULL,
                      pfscan_path = NULL,
                      os = NULL) {
  # 1. Check system dependencies
  checkPerl()

  # 2. Detect OS if not provided
  if (is.null(os)) {
    os <- detectOs()
  }

  # 3. Resolve Input/Output Paths
  if (!file.exists(in_file)) {
    stop("Input file does not exist: ", in_file)
  }
  # FORCE forward slashes to avoid backslash escaping issues in Perl/Shell calls
  in_file <- normalizePath(in_file, winslash = "/")

  out_dir <- normalizePath(dirname(out_file), winslash = "/", mustWork = FALSE)
  out_base <- basename(out_file)
  out_file_clean <- file.path(out_dir, out_base)

  # 4. Resolve Tool Paths (Priority: User Argument > Cache)

  # --- ps_scan.pl ---
  final_ps_scan <- if (!is.null(ps_scan_path)) {
    if (!file.exists(ps_scan_path)) stop("Provided ps_scan_path not found.")
    normalizePath(ps_scan_path, winslash = "/")
  } else {
    getCachedPsScanTool("ps_scan.pl", os)
  }

  # --- prosite.dat ---
  final_db <- if (!is.null(database_path)) {
    if (!file.exists(database_path)) stop("Provided database_path not found.")
    normalizePath(database_path, winslash = "/")
  } else {
    getCachedPsScanTool("prosite.dat", os)
  }

  # --- pfscan (Executable) ---
  final_pfscan <- if (!is.null(pfscan_path)) {
    if (!file.exists(pfscan_path)) stop("Provided pfscan_path not found.")
    normalizePath(pfscan_path, winslash = "/")
  } else {
    if (os != "MAC") {
      getCachedPsScanTool("pfscan", os)
    } else {
      NULL
    }
  }

  # 5. Construct Command
  cmd_args <- constructCommand(
    ps_scan_script = final_ps_scan,
    patterns_db = final_db,
    input_fasta = in_file,
    output_format = out_format,
    pfscan_exec = final_pfscan,
    output_file = out_file_clean
  )

  # 6. Execute
  executeCommand(args = cmd_args, out_file = out_file_clean)
}

#' Check if Perl is available
#' @noRd
checkPerl <- function() {
  perl_path <- Sys.which("perl")
  if (!nzchar(perl_path)) {
    stop("Perl is not found on your system. Please install Perl to use runPsScan.",
         call. = FALSE)
  }
}

#' Get a Cached PS-Scan Tool/Database File
#'
#' Uses BiocFileCache to manage downloading and storing ps_scan files.
#' @param tool_name The specific tool to get: "ps_scan.pl", "prosite.dat", or "pfscan".
#' @param os The operating system ("WIN", "LINUX", "MAC").
#' @return The local path to the cached file.
#' @noRd
getCachedPsScanTool <- function(tool_name, os) {
  cache <-
    BiocFileCache::BiocFileCache(cache = "PMScanR_cache", ask = FALSE)

  base_url <- "https://ftp.expasy.org/databases/prosite/"

  resource_map <- list(
    `ps_scan.pl` = paste0(base_url, "ps_scan/ps_scan.pl"),
    `prosite.dat` = paste0(base_url, "prosite.dat"),
    `pfscan_WIN` = paste0(base_url, "ps_scan/ps_scan_win32.zip"),
    `pfscan_LINUX` = paste0(base_url, "ps_scan/ps_scan_linux_x86_elf.tar.gz"),
    `pfscan_MAC` = paste0(base_url, "ps_scan/ps_scan_macosx.tar.gz")
  )

  query_name <-
    if (tool_name == "pfscan")
      paste0(tool_name, "_", os)
  else
    tool_name

  cache_info <-
    BiocFileCache::bfcquery(cache, query = query_name, field = "rname")

  fpath <- NULL

  if (nrow(cache_info) == 0) {
    message(sprintf(
      "'%s' not found in cache. Downloading from PROSITE...",
      query_name
    ))
    download_url <- resource_map[[query_name]]
    if (is.null(download_url)) {
      stop(sprintf("No download URL defined for '%s'", query_name))
    }
    fpath <-
      BiocFileCache::bfcadd(cache, rname = query_name, fpath = download_url)
    message("Download complete.")
  } else {
    fpath <- cache_info$rpath[1]
  }

  fpath <- normalizePath(fpath, winslash = "/")

  if (tool_name == "pfscan") {
    exec_path_map <- list(WIN = "ps_scan/pfscan.exe",
                          LINUX = "ps_scan/pfscan",
                          MAC = "ps_scan/pfscan")

    exec_rname <- paste0("executable_", query_name)
    exec_cache_info <-
      BiocFileCache::bfcquery(cache, query = exec_rname, field = "rname")

    if (nrow(exec_cache_info) == 0) {
      message(sprintf("Extracting executable from '%s'...", basename(fpath)))
      exdir <- tempfile()
      dir.create(exdir)

      if (os == "WIN") {
        utils::unzip(fpath, exdir = exdir)
      } else {
        utils::untar(fpath, exdir = exdir)
      }

      extracted_file_path <-
        file.path(exdir, exec_path_map[[os]])

      if (!file.exists(extracted_file_path)) {
        stop("Could not find executable after extraction.")
      }

      final_exec_path <-
        BiocFileCache::bfcadd(
          cache,
          rname = exec_rname,
          fpath = extracted_file_path,
          action = "move"
        )

      if (os != "WIN")
        Sys.chmod(final_exec_path, mode = "0755")
      unlink(exdir, recursive = TRUE)

      return(normalizePath(final_exec_path, winslash = "/"))
    } else {
      final_exec_path <- exec_cache_info$rpath[1]
      if (os != "WIN" &&
          file.access(final_exec_path, 1) != 0) {
        Sys.chmod(final_exec_path, mode = "0755")
      }
      return(normalizePath(final_exec_path, winslash = "/"))
    }
  }

  return(fpath)
}

#' Construct Command Arguments for system2
#' @return A character vector of arguments for `system2`.
#' @noRd
constructCommand <- function(ps_scan_script,
                             patterns_db,
                             input_fasta,
                             output_format,
                             pfscan_exec,
                             output_file) {

  args <- c(ps_scan_script,
            "-d",
            patterns_db,
            input_fasta,
            "-o",
            output_format)

  if (is.null(pfscan_exec)) {
    args <- c(args, "--r")
  } else {
    args <- c(args, "--pfscan", pfscan_exec)
  }

  return(args)
}

#' Execute the PS-Scan Command using system2
#' @param args A character vector of command arguments.
#' @param out_file Path to the output file.
#' @return Invisibly returns the command's exit status.
#' @noRd
executeCommand <- function(args, out_file) {
  message("Starting PROSITE analysis...")

  status_code <- system2("perl",
                         args = args,
                         stdout = out_file,
                         stderr = "")

  if (status_code == 0) {
    message("PROSITE analysis finished successfully.")
  } else {
    warning(sprintf(
      "PROSITE analysis failed with exit code %d.",
      status_code
    ),
    call. = FALSE)
    warning("Please ensure Perl is correctly installed and input files are valid.")

    if(file.exists(out_file) && file.size(out_file) == 0) {
      file.remove(out_file)
    }
  }

  invisible(status_code)
}
