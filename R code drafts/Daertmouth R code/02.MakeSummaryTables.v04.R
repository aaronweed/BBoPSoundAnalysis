
# ...................................
# 02.MakeSummaryTables.v03.R
# Matt Ayres, 27 Dec 2024
# Process file manifest to create summary tables for each group
#       of date x location, with entries being number of valid files
#
#....................................
rm(list = ls())     # clear environment
library(here)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(scales)
library(pdftools)
library(png)

workpath_root="//dartfs-hpc/rc/lab/a/AyresM/Root/birdSound_HBEF/2023/"   # to root directory
inFile = "soundManifest_HBVW_2023.csv"
outFile = "soundManifest_HBVW_2023.v01.xlsx"
outSheet = "soundManifest"

setwd(workpath_root)
df1 <- read.csv(inFile)
hist(df1$fileLength.min)
  # Remove rows where fileLength.min > 70 (culls one 300-min sound file from SARA 2023)
    df1 <- df1 %>%
      filter(fileLength.min <= 70)

# Create df2 from df1 by removing any rows in which fileLength.min 
# deviates by more than 5% from the mean of fileLength.min
df2 <- df1 %>%
  filter(abs(fileLength.min - mean(fileLength.min, na.rm = TRUE)) / mean(fileLength.min, na.rm = TRUE) <= 0.05)
  hist(df2$fileLength.min)

# Create df3 from df21 by removing any rows with unusual start times  
  # Calculate the threshold for 5% of the total rows in df1
  threshold <- 0.05 * nrow(df1)
  
  # Count occurrences of each startTime.hhmm in df2
  time_counts <- df2 %>%
    count(startTime.hhmm, name = "count")
  
  # Filter df2 to keep rows with startTime.hhmm that meet the 5% threshold
  df3 <- df2 %>%
    semi_join(time_counts %>% filter(count >= threshold), by = "startTime.hhmm")  
    hist(df3$startTime.hhmm)  

## Culling seems to work as intended
    # Write to Excel
    write_xlsx(list(soundManifest = df3), path = outFile)
     
######################  
# Now make a summary table for each group with rows = location and col = mmdd

    summary_title_base = paste0(df3$area[1], " ", df3$year[1])
    
    # Create summary table for each group
    summary_table <- df3 %>%
      group_by(group, plot, date.mmdd) %>%
      summarise(count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = date.mmdd, values_from = count, values_fill = list(count = 0))
    
    # Set output directory
    output_dir <- workpath_root  # Adjust the path
    
    # Loop over each unique group
    unique_groups <- unique(df3$group)
    nGroups = length(unique_groups)
    
    for (grp in unique_groups) {
      # Filter table for each group
      grp_table <- summary_table %>%
        filter(group == grp) %>%
        select(-group)
      
      # **Remove columns where all entries are 0**
      grp_table <- grp_table[, c(TRUE, colSums(grp_table[, -1]) != 0)]
      
      # **Sort columns (excluding first) by column names**
      sorted_cols <- c(1, order(names(grp_table)[-1]) + 1)  # Keep 1st col, sort rest
      grp_table <- grp_table[, sorted_cols]
      
      # Convert panel to matrix to avoid tibble behavior. Two panels if > 60 days
      upper_panel <- as.matrix(grp_table[, c(1, 2:min(60, ncol(grp_table)))])
      
        # Check if grp_table has more than 61 observations
        if (nrow(grp_table) > 61) {
          # Perform the operation for lower_panel
          lower_panel <- as.matrix(grp_table[, c(1, max(2, 61):ncol(grp_table))])
        } else {
          # Assign an empty matrix to lower_panel if condition is not met
            lower_panel <- matrix(nrow = 0, ncol = 0)
        }  
      
      # Custom theme to remove default striping
      custom_theme <- ttheme_minimal(
        core = list(
          fg_params = list(cex = 0.6),
          bg_params = list(fill = NA, col = NA)  # No default background
        ),
        colhead = list(fg_params = list(cex = 0.9, rot = c(0, rep(90, ncol(upper_panel) - 1))))
      )
      
      # Create table grobs
      # Adjust column ranges dynamically
      upper_cols <- seq(2, min(60, ncol(grp_table)))  # Columns for upper panel
      
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        lower_cols <- seq(max(2, 61), ncol(grp_table)) # Columns for lower panel
      }       
      
      upper_grob <- tableGrob(upper_panel, rows = NULL, theme = custom_theme)
      
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        lower_grob <- tableGrob(lower_panel, rows = NULL, theme = custom_theme)
      }
      
      # Adjust column widths
      upper_grob$widths[1] <- unit(0.7, "in")  # First column wider
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        lower_grob$widths[1] <- unit(0.7, "in")
      }
      
      
      for (i in 2:length(upper_grob$widths)) {
        upper_grob$widths[i] <- unit(0.2, "in")
      }
 
      upper_grob$widths[1] <- unit(0.7, "in")  # First column wider
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        for (i in 2:length(lower_grob$widths)) {
          lower_grob$widths[i] <- unit(0.2, "in")
        }
      }     
      
      # **Adjust row heights (0.3 in for first row, 0.175 in for others)**
      upper_grob$heights <- unit(c(0.3, rep(0.175, nrow(upper_grob) - 1)), "in")
      
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        lower_grob$heights <- unit(c(0.3, rep(0.175, nrow(lower_grob) - 1)), "in")
      }
      
      
      # Apply conditional shading to table cells
      apply_conditional_shading <- function(grob, panel_data) {
        # Define color gradient from white to light blue
        color_palette <- scales::col_numeric(
          palette = c("white", "#87ceeb"),  # Sky blue at max
          domain = c(0, 4)
        )
        
        for (i in 1:nrow(panel_data)) {
          for (j in 2:ncol(panel_data)) {  # Exclude first column
            cell_value <- as.numeric(panel_data[i, j])  # Ensure numeric
            cell_color <- color_palette(cell_value)
            
            # Draw rectangle (background) beneath each text grob
            rect_grob <- rectGrob(
              gp = gpar(fill = cell_color, col = NA)
            )
            
            # Place the rect grob directly below the text
            grob <- gtable_add_grob(
              grob, rect_grob,
              t = i + 1, l = j, b = i + 1, r = j, z = -Inf  # Ensure it's drawn below text
            )
          }
        }
        return(grob)
      }
      
      # Apply shading
      upper_grob <- apply_conditional_shading(upper_grob, upper_panel)
      
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        lower_grob <- apply_conditional_shading(lower_grob, lower_panel)
      }      
      
      
      # Increase font size for the first column (10 pt)
      apply_first_column_font <- function(grob) {
        for (i in which(grob$layout$l == 1)) {  # Loop through first column
          z_index <- grob$layout$z[i]
          
          # Ensure z_index is valid and within bounds
          if (!is.na(z_index) && z_index > 0 && z_index <= length(grob$grobs)) {
            cell_grob <- grob$grobs[[z_index]]
            
            # Apply only to text grobs
            if ("text" %in% class(cell_grob)) {
              grob$grobs[[z_index]]$gp$cex <- 1  # Set 10 pt for first column
            }
          }
        }
      }  
      
      apply_first_column_font(upper_grob)
      if (nrow(grp_table) > 61) {
        # Perform the operation for lower_panel
        apply_first_column_font(lower_grob)
      }
      
      
      # Create PNG output
      png_file <- paste0(output_dir, "summary_", grp, ".png")
      png(png_file, width = 13, height = 8.5, units = "in", res = 300)
      
      # Arrange the panels on the same page, conditionally include lower_grob
      if (nrow(lower_panel) > 0 && ncol(lower_panel) > 0) {
        # Both upper and lower grobs exist
        grid.arrange(
          upper_grob, lower_grob,
          heights = c(0.5, 0.5),
          top = textGrob(paste(summary_title_base, grp), 
                         gp = gpar(fontsize = 18, fontface = "bold"))
        )
      } else {
        # Only upper_grob exists
        grid.arrange(
          upper_grob,
          top = textGrob(paste(summary_title_base, grp), 
                         gp = gpar(fontsize = 18, fontface = "bold"))
        )
      }
      
      dev.off()
    }
    
  #### Create one pdf with summary table for each group
    # Define PDF output path based on summary title
    pdf_file <- paste0(output_dir, summary_title_base, "_summaryTables.pdf")
    
    # List all PNG files that were created
    png_files <- list.files(output_dir, pattern = "summary_.*\\.png", full.names = TRUE)
    
    # Create PDF in landscape format
    pdf(pdf_file, width = 11, height = 8.5)  # Landscape 8.5 x 11 inches
    
    for (png in png_files) {
      # Read the PNG image
      img <- rasterGrob(png::readPNG(png), interpolate = TRUE)
      
      # Plot the image on each page of the PDF
      grid.newpage()
      grid.draw(img)
    }
    
    dev.off()
    
    # Print confirmation
    message(paste("PDF created:", pdf_file))
    
    