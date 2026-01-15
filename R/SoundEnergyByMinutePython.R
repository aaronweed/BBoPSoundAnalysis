
#' Compute minute-scale background sound energy from FLAC recordings using Python
#'
#' This function reads a sound file manifest in which each row contains paths to one to
#' three sequential FLAC audio files from the same recorder and day. It uses
#' Python's \pkg{soundfile} library via \pkg{reticulate} to extract audio samples
#' directly from FLAC files.
#'
#' It can run sequentially or in parallel (via \pkg{future.apply}) and includes
#' basic error handling for unreadable or corrupted FLAC files.
#'
#' @param manifest A data frame containing at least:
#'   \itemize{
#'     \item \code{path} — relative path to FLAC directory;
#'     \item \code{file} — primary FLAC file name;
#'     \item \code{subsequent.file1}, \code{subsequent.file2} — optional FLAC files;
#'     \item \code{area}, \code{year}, \code{group}, \code{date.mmdd};
#'     \item \code{plot}, \code{startTime.hhmm}.
#'   }
#' @param input_dir Root directory containing FLAC files.
#' @param output_dir Directory where JPEGs and CSVs will be written.
#' @param start_filter Only rows with \code{startTime.hhmm == start_filter}
#'   are processed. Default: \code{"0500"}.
#' @param y_limits Numeric vector of length 2 giving y-axis limits for plots.
#' @param max_minutes Maximum number of minutes to analyze per file set.
#' @param parallel Logical; if \code{TRUE}, rows are processed in parallel using
#'   \pkg{future.apply}. You must have a plan set (e.g., \code{future::plan()}).
#'
#' @return Invisibly returns a data frame with columns:
#'   \code{area, year, group, date, plot, startTime.hhmm, minute, log10RMSE, movAvg11}.
#'
#'   Side effects:
#'   \itemize{
#'     \item JPEG plots written to \code{file.path(output_dir, "noise_jpg_all")}.
#'     \item group–year CSVs written to \code{output_dir}.
#'   }
#'
#' @importFrom zoo rollmean
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual ylim ggsave
#' @importFrom utils write.csv
#' @importFrom cli cli_progress_bar cli_progress_update cli_alert_warning cli_alert_success
#' @importFrom future.apply future_lapply
#' @import reticulate
#'
#' @export
SoundEnergyByMinute.py <- function(
    manifest,
    flac_files,
    output_dir,
    start_filter = "0500",
    y_limits = c(2.3, 4.0),
    max_minutes = 180,
    parallel = FALSE
) {
  # Load Python FLAC reader
  py_run_string("
import soundfile as sf

def read_flac_safe(path):
    try:
        samples, rate = sf.read(path)
        return {
            'ok': True,
            'samples': samples.tolist(),
            'rate': rate,
            'path': path
        }
    except Exception as e:
        print(f'Failed to read FLAC: {path}\\nReason: {str(e)}')
        return {
            'ok': False,
            'samples': None,
            'rate': None,
            'path': path
        }
")
  
  # R wrapper for Python FLAC reader
  read_flac_safe <- function(path) {
    result <- py$read_flac_safe(path)
    if (is.null(result)) {
      cli::cli_alert_warning("Python returned NULL for {path}")
      return(list(ok = FALSE, samples = NULL, rate = NA_integer_, path = path))
    }
    list(
      ok = result$ok,
      samples = unlist(result$samples),
      rate = result$rate,
      path = result$path
    )
  }
  
  # Ensure output dirs
  jpg_dir <- file.path(output_dir, "noise_jpg_all")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (!dir.exists(jpg_dir)) dir.create(jpg_dir, recursive = TRUE)
  
  # Filter manifest
  manifest <- dplyr::filter(manifest, startTime.hhmm == start_filter)
  if (nrow(manifest) == 0) {
    cli::cli_alert_warning("No rows match startTime.hhmm == {start_filter}.")
    return(invisible(NULL))
  }
  
  # Helper: process a single manifest row
  process_row <- function(row, y_limits, max_minutes, flac_files, jpg_dir) {
    f1 <- file.path(flac_files, row$path, row$file)
    f2 <- if (!is.na(row$subsequent.file1)) file.path(flac_files, row$path, row$subsequent.file1) else NA_character_
    f3 <- if (!is.na(row$subsequent.file2)) file.path(flac_files, row$path, row$subsequent.file2) else NA_character_
    
    r1 <- read_flac_safe(f1)
    if (!isTRUE(r1$ok)) return(NULL)
    
    audio <- r1$samples
    rate  <- r1$rate
    
    if (!is.na(f2)) {
      r2 <- read_flac_safe(f2)
      if (isTRUE(r2$ok)) audio <- c(audio, r2$samples)
    }
    if (!is.na(f3)) {
      r3 <- read_flac_safe(f3)
      if (isTRUE(r3$ok)) audio <- c(audio, r3$samples)
    }
    
    duration_min <- length(audio) / rate / 60
    minuteMax <- min(floor(duration_min), max_minutes)
    if (minuteMax < 1) return(NULL)
    
    log10RMSE <- numeric(minuteMax)
    for (m in seq_len(minuteMax)) {
      start <- (m - 1) * 60 * rate + 1
      end   <- m * 60 * rate
      seg   <- audio[start:end]
      log10RMSE[m] <- log10(sd(seg))
    }
    
    movAvg11 <- zoo::rollmean(log10RMSE, k = 11, fill = NA)
    
    df_out <- data.frame(
      area = row$area,
      year = row$year,
      group = row$group,
      date = row$date.mmdd,
      plot = row$plot,
      startTime.hhmm = row$startTime.hhmm,
      minute = seq_len(minuteMax),
      log10RMSE = log10RMSE,
      movAvg11 = movAvg11,
      stringsAsFactors = FALSE
    )
    
    # Plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = df_out,
        ggplot2::aes(x = minute, y = log10RMSE, color = "byMinute")
      ) +
      ggplot2::geom_line(
        data = df_out,
        ggplot2::aes(x = minute, y = movAvg11, color = "movAvg11")
      ) +
      ggplot2::labs(
        x = "Minute",
        y = "log10RMSE",
        title = paste(row$area, row$group, row$plot, row$year, row$date.mmdd)
      ) +
      ggplot2::scale_color_manual(values = c(byMinute = "blue", movAvg11 = "red")) +
      ggplot2::ylim(y_limits)
    
    out_jpg <- file.path(
      jpg_dir,
      paste0(row$area, "_", row$group, "_", row$plot, "_",
             row$year, "_", row$date.mmdd, ".jpg")
    )
    ggplot2::ggsave(out_jpg, p, width = 6, height = 4, dpi = 300)
    
    df_out
  }
  
  # Progress bar
  n <- nrow(manifest)
  cli::cli_progress_bar("Analyzing FLAC rows", total = n)
  index <- seq_len(n)
  
  # Choose apply function
  apply_fun <- if (parallel) future.apply::future_lapply else lapply
  
  res_list <- apply_fun(
    index,
    function(i) {
      on.exit(cli::cli_progress_update(), add = TRUE)
      row <- manifest[i, , drop = FALSE]
      process_row(row, y_limits = y_limits, max_minutes = max_minutes,
                  flac_files = flac_files, jpg_dir = jpg_dir)
    }
  )
  
  # Combine results
  res_list <- Filter(Negate(is.null), res_list)
  if (length(res_list) == 0L) {
    cli::cli_alert_warning("No valid noise data produced from manifest.")
    return(invisible(NULL))
  }
  
  noise_out4 <- do.call(rbind, res_list)
  
  # Split and write CSVs
  split_key <- interaction(noise_out4$group, noise_out4$year)
  groups <- split(noise_out4, split_key)
  
  for (nm in names(groups)) {
    out_csv <- file.path(output_dir, paste0("group_", nm, ".csv"))
    utils::write.csv(groups[[nm]], out_csv, row.names = FALSE)
  }
  
  cli::cli_alert_success("Sound energy analysis completed: {length(groups)} group-year CSVs written.")
  invisible(noise_out4)
}
