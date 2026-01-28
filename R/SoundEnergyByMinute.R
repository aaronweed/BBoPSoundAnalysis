#' Calculate minute-level acoustic noise metrics from FLAC recordings
#'
#' This function reads a sound manifest, converts FLAC files to WAV using
#' the external `sox` utility, concatenates sequential audio files,
#' computes per-minute log10(RMSE) values, applies an 11-minute rolling
#' average, generates diagnostic plots, and writes grouped CSV outputs.
#'
#' @param manifest_file A data frame containing the initial sound manifest created by \code{CreateManifest()}, including
#'   at least the columns \code{fileLength.min}, \code{startTime.hhmm},
#'   \code{group}, \code{plot}, \code{date.mmdd}, \code{area}, and \code{year}. Can also be a path to a CSV file containing the manifest.
#' @param manifest_sheet Sheet name in the manifest file.
#' @param flac_dir Directory containing FLAC audio files.
#' @param output_dir Directory of where to save CSV and JPG outputs.
#' @param temp_dir Temporary directory for WAV conversion.
#' @param start_time Filter manifest rows by `startTime.hhmm`. Defaults to 0500.
#' @param y_limits Numeric vector of length 2 giving y-axis limits for plots. Defaults to c(2.3, 4.0).
#'
#' @return A data frame containing minute-level noise metrics for all files. Saves one jpg for each recorder on each day of log10(RMSE) per minute.
#'
#' @details
#' Requires the external program **SoX** to be installed and available on
#' the system PATH. Each FLAC file is temporarily converted to WAV before
#' analysis.
#'
#' Output includes:
#' \itemize{
#'   \item Per-minute log10(RMSE)
#'   \item 11-minute rolling mean
#'   \item JPG plots of noise time series
#'   \item Grouped CSV files by group × year
#' }
#'
#' @export
#' 
SoundEnergyByMinute <- function(
    manifest_file,
    manifest_sheet = "soundManifest",
    flac_dir,
    output_dir,
    temp_dir = tempdir(),
    start_time = 500,
    y_limits = c(2.3, 4.0)
) {
  
  # ---- FLEXIBLE INPUT HANDLING ----
  if (is.character(manifest_file)) {
    # Treat as CSV file path
    if (!file.exists(manifest_file)) {
      stop("CSV file not found: ", manifest_file)
    }
    manifest_file <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
    message("Imported manifest from CSV: ", manifest_file)
    
  } else if (is.data.frame(manifest_file)) {    # Use data frame directly
    manifest_file <- manifest_file
    message("Using manifest supplied as data frame")
    
  } else {
    # Treat as object name (unquoted)
    obj_name <- as.character(substitute(manifest_file))
    if (!exists(obj_name, envir = .GlobalEnv)) {
      stop("Object not found in global environment: ", obj_name)
    }
    manifest_file <- get(obj_name, envir = .GlobalEnv)
    if (!is.data.frame(manifest_file)) {
      stop("Object '", obj_name, "' exists but is not a data frame")
    }
    message("Imported manifest from global environment object: ", obj_name)
  }
  
  ## ---- dependencies ----
  if (Sys.which("sox") == "") {
    stop("SoX is required but not found on system PATH.")
  }
  
  ## ---- # Create output subdirectories if they do not exist ----
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  jpg_dir <- file.path(output_dir, "noise_jpg_all")
  dir.create(jpg_dir, showWarnings = FALSE)
  
  ## ---- read & filter manifest by start time ----
  manifest <- openxlsx::read.xlsx(manifest_file, manifest_sheet) |>
    dplyr::filter(startTime.hhmm == start_time)
  
  ## ---- helper function: flac -> wav -> Wave ----
  # write flacfile and then convert to wav
  read_flac_as_wave <- function(flac_path, wav_path) {
    system2("sox", c(shQuote(flac_path), shQuote(wav_path)), stdout = FALSE)
    tuneR::readWave(wav_path)
  }
  
  all_results <- list()
  
  ## ---- main loop ----
  
 
  for (f in seq_len(nrow(manifest))) {  
    
    if (f %% 100 == 0) message("Processing file ", f) # Check if the loop index is a multiple of 100
    
    unlink(list.files(temp_dir, "^output_file_.*\\.wav$", full.names = TRUE)) # delete any previous versions of temporary wav files in this directory
    
    wav_paths <- file.path( # Assign wav_file within the workpath_temp directory
      temp_dir,
      paste0("output_file_", 1:3, ".wav")
    )
    
    flac_files <- c(
      manifest$file[f],
      manifest$subsequent.file1[f],
      manifest$subsequent.file2[f]
    )
    
    flac_files <- flac_files[!is.na(flac_files)]
    
    waves <- lapply(seq_along(flac_files), function(i) {
      flac_path <- file.path(flac_dir, manifest$path[f], flac_files[i])
      flac_path <- gsub("//", "/", flac_path)
      read_flac_as_wave(flac_path, wav_paths[i]) # write wav file and then read it
    })
    
    audio_full <- unlist(lapply(waves, function(w) w@left)) # combine wav files
    samp_rate <- waves[[1]]@samp.rate # get sample rate from each wav file
    
    minute_max <- floor(length(audio_full) / samp_rate / 60) # truncate any data after the last full minute
    if (minute_max < 2) next 
    
    log10_rmse <- numeric(minute_max)
    
    for (m in seq_len(minute_max)) { # skip first minute
      idx <- ((m) * 60 * samp_rate + 1):((m + 1) * 60 * samp_rate)
      log10_rmse[m] <- log10(stats::sd(audio_full[idx]))
    }
    
    mov_avg <- zoo::rollmean(log10_rmse, k = 11, fill = NA) # calculate moving average for 11-sec
    
    df <- data.frame(
      area = manifest$area[f],
      year = manifest$year[f],
      group = manifest$group[f],
      date = manifest$date.mmdd[f],
      plot = manifest$plot[f],
      startTime.hhmm = manifest$startTime.hhmm[f],
      minute = seq_len(minute_max),
      log10RMSE = log10_rmse,
      movAvg11 = mov_avg,
      stringsAsFactors = FALSE
    )
    
    ## ---- plot ----
    if (nrow(df) > 10) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = minute)) +
        ggplot2::geom_line(ggplot2::aes(y = log10RMSE, color = "byMinute")) +
        ggplot2::geom_line(ggplot2::aes(y = movAvg11, color = "movAvg11")) +
        ggplot2::scale_color_manual(values = c("blue", "red"), name = "Lines") +
        ggplot2::labs(
          title = paste(df$area[1], df$group[1], df$plot[1],
                        df$year[1], df$date[1], sep = " - "),
          x = "Minutes",
          y = "log10(RMSE)"
        ) +
        ggplot2::ylim(y_limits)
      
      ggsave(
        filename = file.path(
          jpg_dir,
          paste0(df$area[1], "-", df$group[1], "-", df$plot[1], ".",
                 df$year[1], "-", df$date[1], ".jpg")
        ),
        plot = p,
        width = 6, height = 4, dpi = 300
      )
    }
    
    all_results[[length(all_results) + 1]] <- df
  }
  
  final_df <- dplyr::bind_rows(all_results)
  
  ## ---- grouped CSV output ----
  groups <- split(final_df, interaction(final_df$group, final_df$year))
  for (nm in names(groups)) {
    utils::write.csv(
      groups[[nm]],
      file.path(output_dir, paste0("group_", nm, ".csv")),
      row.names = FALSE
    )
  }
  
  final_df
}
