#' Run PS-Scan with Caching and Improved Execution
#'
#' This function runs the PROSITE ps_scan tool. It handles the downloading and
#' caching of required executables and databases using BiocFileCache, detects
#' the operating system, and executes the scan in a robust manner.
#'
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
#' @export
runPsScan <- function(in_file, out_file, out_format, os = NULL) {
    # 1. Detect OS if not provided
    if (is.null(os)) {
        os <- detectOs()
    }
    
    # 2. Get paths to required files, downloading and caching them if needed
    # This function now uses BiocFileCache
    ps_scan_path <- getCachedPsScanTool("ps_scan.pl", os)
    patterns_dat_path <- getCachedPsScanTool("prosite.dat", os)
    
    # pfscan is optional for MAC, required for others
    pf_scan_path <- NULL
    if (os != "MAC") {
        pf_scan_path <- getCachedPsScanTool("pfscan", os)
    }
    
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
    executeCommand(args = cmd_args,
                   os = os,
                   out_file = out_file)
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
    
    if (nrow(cache_info) == 0) {
        # File not in cache, download it
        message(sprintf("'%s' not found in cache. Downloading...", query_name))
        download_url <- resource_map[[query_name]]
        if (is.null(download_url)) {
            stop(sprintf("No download URL defined for '%s'", query_name))
        }
        fpath <-
            BiocFileCache::bfcadd(cache, rname = query_name, fpath = download_url)
        message("Download complete.")
    } else {
        # File is in cache, get its path
        fpath <- cache_info$rpath[1]
    }
    
    # Handle extraction for compressed pfscan archives
    if (tool_name == "pfscan") {
        exec_path_map <- list(WIN = "ps_scan/pfscan.exe",
                              LINUX = "ps_scan/pfscan",
                              MAC = "ps_scan/pfscan")
        
        # The cache path for the pfscan executable itself
        exec_rname <- paste0("executable_", query_name)
        exec_cache_info <-
            BiocFileCache::bfcquery(cache, exec_rname)
        
        if (nrow(exec_cache_info) == 0) {
            message(sprintf(
                "Extracting executable from '%s'...",
                basename(fpath)
            ))
            exdir <- tempfile()
            dir.create(exdir)
            
            if (os == "WIN") {
                utils::unzip(fpath, exdir = exdir)
            } else {
                utils::untar(fpath, exdir = exdir)
            }
            
            # The full path to the extracted executable
            extracted_file_path <-
                file.path(exdir, exec_path_map[[os]])
            if (!file.exists(extracted_file_path))
                stop("Could not find executable after extraction.")
            
            # Add the extracted executable to the cache for future use
            final_exec_path <-
                BiocFileCache::bfcadd(cache, rname = exec_rname, fpath = extracted_file_path)
            
            # Make it executable on Linux/Mac
            if (os != "WIN")
                Sys.chmod(final_exec_path, mode = "0755")
            
            return(final_exec_path)
        } else {
            # Return path to already cached executable
            final_exec_path <- exec_cache_info$rpath[1]
            if (os != "WIN" &&
                file.access(final_exec_path, 1) != 0) {
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
constructCommand <-
    function(ps_scan_script,
             patterns_db,
             input_fasta,
             output_format,
             pfscan_exec,
             output_file) {
        # Base arguments for the perl script
        args <- c(ps_scan_script,
                  "-d",
                  patterns_db,
                  input_fasta,
                  "-o",
                  output_format)
        
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
    status_code <- system2(command,
                           args = args,
                           stdout = out_file,
                           stderr = "")
    
    if (status_code == 0) {
        message(
            sprintf(
                "PROSITE analysis finished successfully. Results saved to: %s",
                out_file
            )
        )
    } else {
        warning(sprintf(
            "PROSITE analysis may have failed with exit code %d.",
            status_code
        ))
        warning("Check that Perl is installed and in your system's PATH.")
    }
    
    invisible(status_code)
}
