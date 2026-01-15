#' Calculate minute-level acoustic noise metrics from FLAC recordings
#'
#' This function reads a sound manifest, converts FLAC files to WAV using
#' the external `sox` utility, concatenates sequential audio files,
#' computes per-minute log10(RMSE) values, applies an 11-minute rolling
#' average, generates diagnostic plots, and writes grouped CSV outputs.
#'
#' @param manifest_file A data frame containing the initial sound manifest created by \code{CreateManifest()}, including
#'   at least the columns \code{fileLength.min}, \code{startTime.hhmm},
#'   \code{group}, \code{plot}, \code{date.mmdd}, \code{area}, and \code{year}.
#' @param manifest_sheet Sheet name in the manifest file.
#' @param flac_root Root directory containing FLAC audio files.
#' @param output_dir Directory for CSV and JPG outputs.
#' @param temp_dir Temporary directory for WAV conversion.
#' @param start_time Filter manifest rows by `startTime.hhmm`.
#' @param y_limits Numeric vector of length 2 giving y-axis limits for plots.
#'
#' @return A data frame containing minute-level noise metrics for all files.
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
    flac_root,
    output_dir,
    temp_dir = tempdir(),
    start_time = 500,
    y_limits = c(2.3, 4.0)
) {
  
  ## ---- dependencies ----
  if (Sys.which("sox") == "") {
    stop("SoX is required but not found on system PATH.")
  }
  
  ## ---- directories ----
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  jpg_dir <- file.path(output_dir, "noise_jpg_all")
  dir.create(jpg_dir, showWarnings = FALSE)
  
  ## ---- read & filter manifest ----
  manifest <- openxlsx::read.xlsx(manifest_file, manifest_sheet) |>
    dplyr::filter(startTime.hhmm == start_time)
  
  ## ---- helper: flac -> wav -> Wave ----
  read_flac_as_wave <- function(flac_path, wav_path) {
    system2("sox", c(shQuote(flac_path), shQuote(wav_path)), stdout = FALSE)
    tuneR::readWave(wav_path)
  }
  
  all_results <- list()
  
  ## ---- main loop ----
  for (f in seq_len(nrow(manifest))) {
    
    if (f %% 100 == 0) message("Processing file ", f)
    
    unlink(list.files(temp_dir, "^output_file_.*\\.wav$", full.names = TRUE))
    
    wav_paths <- file.path(
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
      flac_path <- file.path(flac_root, manifest$path[f], flac_files[i])
      flac_path <- gsub("//", "/", flac_path)
      read_flac_as_wave(flac_path, wav_paths[i])
    })
    
    audio_full <- unlist(lapply(waves, function(w) w@left))
    samp_rate <- waves[[1]]@samp.rate
    
    minute_max <- floor(length(audio_full) / samp_rate / 60)
    if (minute_max < 2) next
    
    log10_rmse <- numeric(minute_max)
    
    for (m in seq_len(minute_max)) {
      idx <- ((m) * 60 * samp_rate + 1):((m + 1) * 60 * samp_rate)
      log10_rmse[m] <- log10(stats::sd(audio_full[idx]))
    }
    
    mov_avg <- zoo::rollmean(log10_rmse, k = 11, fill = NA)
    
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
