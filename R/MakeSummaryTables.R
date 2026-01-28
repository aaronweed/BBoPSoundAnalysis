#' Filter recordings by maximum duration
#'
#' Removes rows where \code{fileLength.min} exceeds a specified maximum duration.
#'
#' @param df A data frame containing a numeric column \code{fileLength.min}.
#' @param max_minutes Numeric scalar. Maximum allowed duration in minutes.
#'
#' @return A filtered data frame with only rows where
#'   \code{fileLength.min <= max_minutes}.
#'
#' @keywords internal
#' @noRd
filter_by_duration <- function(df, max_minutes = 70) {
  cli::cli_progress_step("Filtering long recordings")
  df<-dplyr::filter(df, .data$fileLength.min <= max_minutes)
}

#' Filter recordings by deviation from mean duration
#'
#' Retains rows where \code{fileLength.min} deviates from the mean duration
#' by no more than a specified percentage.
#'
#' @param df A data frame containing a numeric column \code{fileLength.min}.
#' @param pct Numeric scalar between 0 and 1. Maximum allowed relative
#'   deviation from the mean duration.
#'
#' @return A filtered data frame.
#'
#' @keywords internal
#' @noRd
filter_by_deviation <- function(df, pct = 0.05) {
  cli::cli_progress_step("Filtering by duration deviation")
  m <- mean(df$fileLength.min, na.rm = TRUE)
  dplyr::filter(df, abs(.data$fileLength.min - m) / m <= pct) #removing any rows in which fileLength.min deviates by more than 5% from the mean of fileLength.min
}

#' Filter recordings by start time frequency
#'
#' Retains rows whose \code{startTime.hhmm} occurs at least a given fraction
#' of the total rows in a reference data frame.
#'
#' @param df A data frame containing \code{startTime.hhmm}.
#' @param reference_df A data frame used to compute the frequency threshold.
#' @param pct Numeric scalar between 0 and 1.
#'
#' @return A filtered data frame.
#'
#' @keywords internal
#' @noRd
filter_by_start_time_frequency <- function(df, reference_df, pct = 0.05) {
  cli::cli_progress_step("Filtering unusual start times")
  
  threshold <- pct * nrow(reference_df)
  
  time_counts <- df |>
    dplyr::count(.data$startTime.hhmm, name = "count")
  
  df |>
    dplyr::semi_join(
      dplyr::filter(time_counts, .data$count >= threshold),
      by = "startTime.hhmm"
    )
}


#' Create summary table of recording counts
#'
#' Builds a wide summary table with rows representing locations (group/plot)
#' and columns representing dates.
#'
#' @param df A data frame containing \code{group}, \code{plot}, and
#'   \code{date.mmdd}.
#'
#' @return A wide data frame with counts per date.
#'
#' @keywords internal
#' @noRd
create_summary_table <- function(df) {
  cli::cli_progress_step("Creating summary table")
  
  df |>
    dplyr::group_by(group, plot, date.mmdd) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = date.mmdd,
      values_from = count,
      values_fill = list(count = 0)
    )
}


#' Generate a PNG summary table for a group
#'
#' Creates a PNG image containing a summary table for a single group, with
#' conditional shading and layout adjustments. If the table spans many
#' date columns, it is split into upper and lower panels.
#'
#' @param grp_table A data frame containing one group of the summary table,
#'   including a first column (e.g., \code{plot}) and date columns.
#' @param grp Character scalar. Group identifier used in the title and
#'   file name.
#' @param title_base Character scalar. Base title text (e.g., area and year).
#' @param output_dir Directory where the PNG file will be written.
#'
#' @return Invisibly returns the path to the created PNG file.
#'
#' @keywords internal
#' @noRd
#' 
generate_group_summary_png <- function(grp_table, grp, title_base, output_dir) {
  cli::cli_progress_step(paste("Rendering summary for group", grp))
  
  # Remove all-zero columns (except first)
  grp_table <- grp_table[, c(TRUE, colSums(grp_table[, -1, drop = FALSE]) != 0)]
  
  # Sort date columns (excluding first)
  sorted_cols <- c(1, order(names(grp_table)[-1]) + 1)
  grp_table <- grp_table[, sorted_cols]
  
  # Split into upper and lower panels if many date columns
  upper_panel <- as.matrix(grp_table[, c(1, 2:min(60, ncol(grp_table))), drop = FALSE])
  lower_panel <- if (ncol(grp_table) > 61) {
    as.matrix(grp_table[, c(1, 61:ncol(grp_table)), drop = FALSE])
  } else {
    NULL
  }
  
  custom_theme <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(cex = 0.6),
      bg_params = list(fill = NA, col = NA)
    ),
    colhead = list(
      fg_params = list(cex = 0.9,
                       rot = c(0, rep(90, ncol(upper_panel) - 1)))
    )
  )
  
  upper_grob <- gridExtra::tableGrob(upper_panel, rows = NULL, theme = custom_theme)
  if (!is.null(lower_panel)) {
    lower_grob <- gridExtra::tableGrob(lower_panel, rows = NULL, theme = custom_theme)
  }
  
  # Simple column width adjustments
  upper_grob$widths[1] <- grid::unit(0.7, "in")
  for (i in 2:length(upper_grob$widths)) {
    upper_grob$widths[i] <- grid::unit(0.2, "in")
  }
  if (!is.null(lower_panel)) {
    lower_grob$widths[1] <- grid::unit(0.7, "in")
    for (i in 2:length(lower_grob$widths)) {
      lower_grob$widths[i] <- grid::unit(0.2, "in")
    }
  }
  
  # Row heights
  upper_grob$heights <- grid::unit(c(0.3, rep(0.175, nrow(upper_grob) - 1)), "in")
  if (!is.null(lower_panel)) {
    lower_grob$heights <- grid::unit(c(0.3, rep(0.175, nrow(lower_grob) - 1)), "in")
  }
  
  # Output file
  png_file <- file.path(output_dir, paste0("summary_", grp, ".png"))
  grDevices::png(png_file, width = 13, height = 8.5, units = "in", res = 300)
  
  if (!is.null(lower_panel)) {
    gridExtra::grid.arrange(
      upper_grob,
      lower_grob,
      heights = c(0.5, 0.5),
      top = grid::textGrob(
        paste(title_base, grp),
        gp = grid::gpar(fontsize = 18, fontface = "bold")
      )
    )
  } else {
    gridExtra::grid.arrange(
      upper_grob,
      top = grid::textGrob(
        paste(title_base, grp),
        gp = grid::gpar(fontsize = 18, fontface = "bold")
      )
    )
  }
  
  grDevices::dev.off()
  invisible(png_file)
}


#' Combine PNG summary tables into a PDF
#'
#' Reads all PNG summary files in a directory and combines them into a single
#' multi‑page PDF document.
#'
#' @param output_dir Directory containing the PNG files.
#' @param title_base Character scalar used to construct the PDF file name.
#'
#' @return Invisibly returns the path to the created PDF file.
#'
#' @keywords internal
#' @noRd
combine_pngs_to_pdf <- function(output_dir, title_base) {
  cli::cli_progress_step("Combining PNGs into PDF")
  
  png_files <- list.files(output_dir, pattern = "summary_.*\\.png", full.names = TRUE)
  pdf_file <- file.path(output_dir, paste0(title_base, "_summaryTables.pdf"))
  
  grDevices::pdf(pdf_file, width = 11, height = 8.5)
  
  for (png_path in png_files) {
    img <- png::readPNG(png_path)
    grid::grid.newpage()
    grid::grid.draw(grid::rasterGrob(img, interpolate = TRUE))
  }
  
  grDevices::dev.off()
  message("PDF created: ", pdf_file)
  invisible(pdf_file)
}


#' Make Summary Tables per Group
#'
#' Performs cleaning, filtering, and summarization of a sound manifest,
#' writes a cleaned manifest to Excel, and generates per-group summary
#' tables as PNG images and a combined PDF.
#'
#' @param manifest_file A data frame produced by \code{CreateManifest()} or
#'   a path to a CSV file containing the manifest.
#' @param manifest_xlsx Path to an Excel file where the cleaned manifest
#'   will be written.
#' @param output_dir Directory where PNGs and the combined PDF are written.
#' @param title_base Character string used as the base title for summary plots.
#'
#' @return Invisibly returns a list with components:
#'   \itemize{
#'     \item \code{df1} After filtering long recordings
#'     \item \code{df2} After deviation filtering
#'     \item \code{df3} Final cleaned dataset
#'     \item \code{summary_table} Wide summary table
#'   }
#'
#' @examples
#' \dontrun{
#' MakeSummaryTables(
#'   manifest_file = sound_manifest,
#'   manifest_xlsx = "cleaned_manifest.xlsx",
#'   output_dir = "output/",
#'   title_base = "BBoP Deployment Summary"
#' )
#' }
#'
#' @export
#' 
MakeSummaryTables <- function(manifest_file,
                              manifest_xlsx,
                              output_dir) {
  
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
  
  # ---- MAIN PIPELINE ----
  cli::cli_progress_bar("Processing sound manifest", total = 5)
  
  df1_filtered <- filter_by_duration(manifest_file)                  ; cli::cli_progress_update()
  df2 <- filter_by_deviation(df1_filtered)                 ; cli::cli_progress_update()
  df3 <- filter_by_start_time_frequency(df2, df1_filtered) ; cli::cli_progress_update()
  
  summary_table <- create_summary_table(df3)               ; cli::cli_progress_update()
  
  writexl::write_xlsx(df3, manifest_xlsx)
  
  title_base <- paste(df3$area[1], df3$year[1])
  
  for (grp in unique(df3$group)) {
    grp_table <- summary_table |>
      dplyr::filter(group == grp) |>
      dplyr::select(-group)
    
    generate_group_summary_png(grp_table, grp, title_base, output_dir)
  }
  
  combine_pngs_to_pdf(output_dir, title_base)
  cli::cli_progress_update()
  
  invisible(list(
    manifest_file = df1_filtered,
    df2 = df2,
    df3 = df3,
    summary_table = summary_table
  ))
}

