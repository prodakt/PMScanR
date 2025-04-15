#' Retrieve Logical Drives Based on Operating System
#'
#' This function detects the user's operating system and retrieves a list of logical drives or volumes.
#' It supports macOS (Darwin), Linux, and Windows. The function returns a named character vector where
#' the names are human-readable labels (e.g., volume names or drive letters), and the values are the
#' corresponding paths.
#'
#' @return A named character vector with drive labels as names and paths as values. Returns an empty
#' named character vector if no drives are found or if parsing fails.
#'
#' @details
#' - On macOS (Darwin), volumes are retrieved from the "/Volumes" directory.
#' - On Linux, the root directory ("/") is included, along with any mounted media from "/media" if available.
#' - On Windows, drives are retrieved using WMIC if available; otherwise, PowerShell is used as a fallback.
#'
#' @examples
#' {
#'    getLogicalDrives()
#' }
#'
#' @export
getLogicalDrives <- function() {
  osType <- Sys.info()["sysname"]

  if (osType == "Darwin") {
    # macOS: List volumes in /Volumes
    logicalDrives <- list.dirs("/Volumes", full.names = TRUE, recursive = FALSE)
    names(logicalDrives) <- basename(logicalDrives)
  } else if (osType == "Linux") {
    # Linux: Include root and media if available
    logicalDrives <- c(Computer = "/")
    if (dir.exists("/media")) {
      mediaDirs <- list.dirs("/media", full.names = TRUE, recursive = FALSE)
      names(mediaDirs) <- basename(mediaDirs)
      logicalDrives <- c(logicalDrives, mediaDirs)
    }
  } else if (osType == "Windows") {
    # Windows: Try WMIC first, fallback to PowerShell if needed
    wmicPath <- file.path(Sys.getenv("SystemRoot"), "System32", "Wbem", "WMIC.exe")
    useWMIC <- FALSE
    if (file.exists(wmicPath)) {
      # Use WMIC to retrieve drive information
      captionCommand <- paste(wmicPath, "logicaldisk get Caption")
      volumeNameCommand <- paste(wmicPath, "logicaldisk get VolumeName")

      driveCaptionsOutput <- system(captionCommand, intern = TRUE, ignore.stderr = TRUE)
      captionStatus <- attr(driveCaptionsOutput, "status")
      driveCaptions <- sub(" *\\r$", "", driveCaptionsOutput)
      driveCaptions <- driveCaptions[!tolower(driveCaptions) %in% c("caption", "")]

      volumeNamesOutput <- system(volumeNameCommand, intern = TRUE, ignore.stderr = TRUE)
      volumeNameStatus <- attr(volumeNamesOutput, "status")
      volumeNames <- sub(" *\\r$", "", volumeNamesOutput)
      volumeNames <- volumeNames[!tolower(volumeNames) %in% c("volumename", "")]

      if (is.null(captionStatus) && is.null(volumeNameStatus) &&
          length(driveCaptions) > 0 && length(volumeNames) > 0 &&
          length(driveCaptions) == length(volumeNames)) {
        driveNames <- paste0(volumeNames, ifelse(volumeNames == "", "", " "), "(", driveCaptions, ")")
        logicalDrives <- gsub(":$", ":/", driveCaptions)
        if (length(logicalDrives) == length(driveNames)) {
          names(logicalDrives) <- driveNames
          useWMIC <- TRUE
        }
      }
    }

    if (!useWMIC) {
      # Fallback to PowerShell
      driveInfo <- system2("powershell",
                           "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                           stdout = TRUE)
      driveCount <- as.integer(driveInfo[1])
      if (is.na(driveCount) || driveCount <= 0) {
        logicalDrives <- character(0)
        names(logicalDrives) <- character(0)
      } else {
        driveData <- driveInfo[-1]
        # Oddziel ścieżki i etykiety
        drivePaths <- driveData[1:driveCount]
        volumeLabels <- driveData[(driveCount + 1):length(driveData)]
        # Uzupełnij brakujące etykiety pustymi ciągami
        if (length(volumeLabels) < driveCount) {
          volumeLabels <- c(volumeLabels, rep("", driveCount - length(volumeLabels)))
        }
        # Przetwarzanie danych
        drivePaths <- gsub(":\\\\$", ":/", drivePaths)
        noLabel <- volumeLabels == ""
        volumeLabels[noLabel] <- gsub(":/$", ":", drivePaths[noLabel])
        logicalDrives <- drivePaths
        driveNames <- paste0(volumeLabels, " (", gsub(":/$", ":", logicalDrives), ")")
        names(logicalDrives) <- driveNames
      }
    }
  } else {
    stop("Unsupported OS")
  }

  return(logicalDrives)
}
