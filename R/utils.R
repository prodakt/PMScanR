#' Detect the operating system
#'
#' An internal helper function to get the OS type.
#'
#' @return A string indicating the OS ("WIN", "LINUX", "MAC", or "Unsupported").
#' @noRd
detectOs <- function() {
    sys_info <- tolower(Sys.info()["sysname"])
    if (grepl("windows", sys_info))
        return("WIN")
    if (grepl("linux", sys_info))
        return("LINUX")
    if (grepl("darwin", sys_info))
        return("MAC")
    stop("Unsupported operating system detected.")
}

#' Retrieve Logical Drives Based on Operating System
#'
#' This is an internal helper function for the Shiny app. It detects the user's
#' operating system and retrieves a list of logical drives or volumes.
#'
#' @return A named character vector with drive labels as names and paths as
#'   values.
#' @noRd
getLogicalDrives <- function() {
    osType <- Sys.info()["sysname"]
    
    if (osType == "Darwin") {
        logicalDrives <-
            list.dirs("/Volumes",
                      full.names = TRUE,
                      recursive = FALSE)
        names(logicalDrives) <- basename(logicalDrives)
        
    } else if (osType == "Linux") {
        logicalDrives <- c(Computer = "/")
        if (dir.exists("/media")) {
            mediaDirs <-
                list.dirs("/media",
                          full.names = TRUE,
                          recursive = FALSE)
            names(mediaDirs) <- basename(mediaDirs)
            logicalDrives <- c(logicalDrives, mediaDirs)
        }
        
    } else if (osType == "Windows") {
        wmicPath <-
            file.path(Sys.getenv("SystemRoot"),
                      "System32",
                      "Wbem",
                      "WMIC.exe")
        
        if (file.exists(wmicPath)) {
            driveCaptions <- tryCatch(
                system2(
                    wmicPath,
                    args = "logicaldisk get Caption",
                    stdout = TRUE,
                    stderr = FALSE
                ),
                error = function(e)
                    character(0)
            )
            volumeNames <- tryCatch(
                system2(
                    wmicPath,
                    args = "logicaldisk get VolumeName",
                    stdout = TRUE,
                    stderr = FALSE
                ),
                error = function(e)
                    character(0)
            )
            
            driveCaptions <-
                driveCaptions[driveCaptions != "" &
                                  !grepl("Caption", driveCaptions)]
            volumeNames <-
                volumeNames[volumeNames != "" &
                                !grepl("VolumeName", volumeNames)]
            
            if (length(driveCaptions) > 0 &&
                length(driveCaptions) == length(volumeNames)) {
                driveNames <- paste0(volumeNames, " (", driveCaptions, ")")
                logicalDrives <- paste0(driveCaptions, "/")
                names(logicalDrives) <- driveNames
                return(logicalDrives)
            }
        }
        
        # Fallback to PowerShell if WMIC fails or is not available
        ps_command <-
            "[System.IO.DriveInfo]::GetDrives() | ForEach-Object { $_.Name + ',' + $_.VolumeLabel }"
        driveInfo <- tryCatch(
            system2(
                "powershell",
                args = c("-Command", ps_command),
                stdout = TRUE,
                stderr = FALSE
            ),
            error = function(e)
                character(0)
        )
        
        if (length(driveInfo) > 0) {
            drive_df <- do.call(rbind, strsplit(driveInfo, ","))
            logicalDrives <- gsub("\\\\", "/", drive_df[, 1])
            driveNames <-
                paste0(drive_df[, 2], " (", gsub("/$", "", logicalDrives), ")")
            names(logicalDrives) <- driveNames
        } else {
            logicalDrives <- character(0)
        }
        
    } else {
        warning("Unsupported OS for drive detection. Returning empty list.")
        logicalDrives <- character(0)
    }
    
    return(logicalDrives)
}
