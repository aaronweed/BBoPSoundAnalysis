#' @title Create a Manifest of Sound Files
#'
#' @importFrom exifr read_exif
#' @importFrom cli cli_progress_bar
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom digest digest
#' @importFrom stringr str_sub
#' @importFrom utils write.csv
#' @description This function scans a directory of .flac sound files and creates a manifest with metadata including file size, duration, time stamps (parsed from file
#' names), plot/group identifiers, and MD5 checksums. It assumes a file naming convention where: Date-time stamp starts 20 characters from the end of the filename. Group ID = first 4 characters of the filename. Plot ID = characters 5–8 of the filename.Files are in chronological order.
#'
#' @param input_dir Character. Path to the directory containing `.flac` files named as SITEID_YYYYMMDD_HHHHHH.flac
#' @param output_dir Character. Path where the manifest CSV will be saved.
#' @param year Integer. Year of recording (default = current year).
#' @param location Character. Area, site or prefix (default = `""`).
#' @param technician Character. Name/ID of technician.
#' @param recorderType Character. Recorder type (default = `"SwiftOne"`).
#' @param output_prefix Character. Prefix for the output file (default = `"soundManifest_"`).
#' @param export Select whether to export as \code{csv} file in your output_dir or to the global environment \code{object}.
#'
#' @return A data frame with one row per sound file and the following columns:
#' \itemize{
#'   \item \code{path} – relative directory of file
#'   \item \code{file} – file name
#'   \item \code{subsequent.file1}, \code{subsequent.file2} – possible subsequent files
#'   \item \code{FileSize.MB}, \code{FileType}, \code{recorderType}, \code{area}, \code{group}, \code{plot}
#'   \item \code{plot.alias}, \code{recorder}, \code{recorderAlias}, \code{extraAttrib1}, \code{extraAttrib2}
#'   \item \code{year}, \code{technician}, \code{date.mmdd}, \code{startTime.hhmm},
#'   \item \code{startTime.Excel}, \code{startTime.yyyymmddhhmm}, \code{fileLength.min}
#'   \item \code{MD5_checksum}
#' }
#'
#'
#' @examples
#' \dontrun{
#' CreateManifest(
#'   input_dir = "G:/HB",
#'   output_dir = "C:/output",
#'   year = 2023,
#'   location = "Habitat",
#'   technician = "ASW"
#' )
#' head(manifest)
#' }
#'
#' @export


CreateManifest <- function(input_dir,
                           output_dir,
                           year = as.integer(format(Sys.Date(), "%Y")),
                           location = "",
                           technician = "",
                           recorderType = "SwiftOne",
                           output_prefix = "soundManifest_", 
                           export = c("csv", "object")) {

  # read exif metadata
  
  filePattern <- "\\.flac$"

  #filePattern <- list.files( pattern = "\\.(flac|wav)$", full.names = TRUE )# allows user to return .flac and .wav files
  
  cli::cli_progress_bar("Reading EXIF data from files ")

  a <- exifr::read_exif(
    list.files(input_dir, recursive = TRUE,
               pattern = filePattern, full.names = TRUE,
               ignore.case = TRUE))
  cli::cli_progress_update()


  file_basename <- a$FileName
  FileSize.MB <- a$FileSize / 1000000
  fileLength.min <- a$Duration / 60
  nFiles <- nrow(a)

  # metadata arrays
  FileType <- tolower(a$FileTypeExtension)
  technician <- rep(technician, nFiles)
  recorderType <- rep(recorderType, nFiles)
  plot.alias <- recorder <- recorderAlias <- extraAttrib1 <- extraAttrib2 <- ""
  directoryShort <- subsequent.file1_basename <- subsequent.file2_basename <- MD5_checksum <- rep("", nFiles)

  # parse datetime from filename
  
  clean_filename <- gsub("\\(-\\d{4}\\)", "", file_basename)   # Remove the UTC offset pattern if present
 
  year_date_time <- stringr::str_sub(clean_filename, -20, -6)
  year <- stringr::str_sub(year_date_time, 1, 4)
  month <- stringr::str_sub(year_date_time, 5, 6)
  day <- stringr::str_sub(year_date_time, 7, 8)
  hour <- stringr::str_sub(year_date_time, 10, 11)
  minute <- stringr::str_sub(year_date_time, 12, 13)

  startTime.Excel <- stringr::str_c(month, "-", day, "-", year, " ", hour, ":", minute)
  date.mmdd <- paste(month, day, sep = "")
  startTime.hhmm <- paste(hour, minute, sep = "")
  startTime.yyyymmddhhmm <- paste(year, month, day, hour, minute, sep = "")

  # group/plot from filename

  df <- tibble::tibble(names = stringr::str_sub(clean_filename, 1, -21)) |>
    dplyr::mutate(group = stringr::str_sub(names, 1, 4),
                  plot = stringr::str_sub(names, 5, 8),
                  area = location)

  # create relative dirs & md5 checksums

  for (f in seq_len(nFiles)) {

    full <- a$SourceFile[f]
    fname <- a$FileName[f]

    start <- nchar(input_dir)
    stop  <- nchar(full) - nchar(fname)

    # ensure valid range
    if (start <= stop) {
      ds <- substr(input_dir, 3, nchar(input_dir))   # file is directly in input_dir
    } else {
      
      ds <- substr(full, start, stop)
    }

    # normalize: remove trailing slash if present
    ds <- sub("/$", "", ds)

    directoryShort[f] <- ds

    MD5_checksum[f] <- digest::digest(file(full, "rb"), algo = "md5")

  }

  # check for subsequent file1

  cli::cli_progress_bar("Checking subsequent files (1)", total = nFiles - 1)

  for (f in seq_len(nFiles - 1)) {
    if (date.mmdd[f + 1] == date.mmdd[f] &&
        !is.na(fileLength.min[f]) && !is.na(fileLength.min[f + 1])) {
      tDiff <- as.double(hour[f + 1]) * 60 + as.double(minute[f + 1]) -
        (as.double(hour[f]) * 60 + as.double(minute[f]))
      if (tDiff - fileLength.min[f] < 2) {
        subsequent.file1_basename[f] <- clean_filename[f + 1]
      }
    }
    cli::cli_progress_update()
  }

  # check for subsequent file 2
  cli::cli_progress_bar("Checking subsequent files (2)", total = nFiles - 2)
  for (f in seq_len(nFiles - 2)) {
    if (date.mmdd[f + 2] == date.mmdd[f] &&
        !is.na(fileLength.min[f]) && !is.na(fileLength.min[f + 2])) {
      tDiff <- as.double(hour[f + 2]) * 60 + as.double(minute[f + 2]) -
        (as.double(hour[f]) * 60 + as.double(minute[f]))
      if (tDiff - fileLength.min[f] - fileLength.min[f + 2] < 2) {
        subsequent.file2_basename[f] <- clean_filename[f + 2]
      }
    }
    cli::cli_progress_update()
  }

  # assemble output
  column_names <- c("path","file","subsequent.file1","subsequent.file2",
                    "FileSize.MB","FileType","recorderType","area","group","plot",
                    "plot.alias","recorder","recorderAlias","extraAttrib1","extraAttrib2",
                    "year","technician","date.mmdd","startTime.hhmm",
                    "startTime.Excel","startTime.yyyymmddhhmm","fileLength.min","MD5_checksum")

  manifest <- data.frame(setNames(list(
    directoryShort, clean_filename, subsequent.file1_basename, subsequent.file2_basename,
    FileSize.MB, FileType, recorderType, df$area, df$group, df$plot,
    plot.alias, recorder, recorderAlias, extraAttrib1, extraAttrib2,
    year, technician, date.mmdd, startTime.hhmm, startTime.Excel,
    startTime.yyyymmddhhmm, fileLength.min, MD5_checksum
  ), column_names))

  
  if (export == "csv") { # save output
    outputFile <- file.path(output_dir, paste0(output_prefix, year[1], ".csv"))
    utils::write.csv(manifest, outputFile, row.names = FALSE)
    return(invisible(manifest)) 
    } 
  
  if (export == "object") { return(b1) # returned to global environment if user assigns it }
    
  }

}
