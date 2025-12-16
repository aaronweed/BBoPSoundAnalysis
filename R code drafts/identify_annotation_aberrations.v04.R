rm(list = ls())     # clear environment

# Load necessary library
library(dplyr)
library(exifr)
library(writexl)
library(tidyr)
library(readxl)
library(plotly)
library(patchwork)
library(magick)
library(gridExtra)
library(grid)
library(tidyverse)
library(cowplot)
library(scales)
library(magick)
library(pdftools)
library(readr)
library(jpeg)

# ...............................................
#
# Starts with a folder of annotation files
#
# 10 dec 2024
#
#................................................

workpath_root = 'C:/Ayres/Ayres.Research/BBoP/2024-10'   # to root directory
directory_path <- "humanAnnotations.n448"
inFile1 = "MABI2022_SeasonLong_Annotations_metadata.v01.xlsx"
inSheet1 = "Recordings"
outFile1 = "observed_songs_per_bout.xlsx"
outFile2= "Annotations_all.xlsx"
threshold_min_time_to_next = 5
threshold_min_start = 30
duration_bout = 120
  threshold_max_start = 600 - duration_bout

setwd(workpath_root)

# Define folder and output file
image_folder <- "aberr_out"
output_pdf <- "aberr_summary.pdf"


## create directory for output graphs
if (!dir.exists(image_folder)) {
  dir.create(image_folder)
}

# Step 1: Identify all .txt files in the specified directory

outFile <- paste0("Summary.", directory_path, ".xlsx")

file_list <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)

# Step 2: Initialize an empty list to store the data from each file
data_list <- list()

# Step 3: Loop through each file and read the tab-delimited data
for (file in file_list) {
  # Step 4: Read the tab-delimited text file into a data frame with additional options
  data <- tryCatch(
    {
      read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE, fill = TRUE, quote = "\"")
    },
    error = function(e) {
      message(paste("Error reading file:", file))
      NULL
    }
  )
  
  # Step 5: Add a new column "fileID" containing the first four characters of the file name (without the path)
  if (!is.null(data)) {
    # Extract the base file name (without the path)
    file_name <- basename(file)
    
    # Extract the first four characters of the file name
    file_id <- substr(file_name, 1, 4)
    
    # Add the new column "fileID" to the data frame
    data$fileID <- file_id
    
    # Add the new column "fileID" to the data frame
    data$fileID <- file_id
    
    # Append the data frame to the list
    data_list[[length(data_list) + 1]] <- data
  }
}

# Step 6: Concatenate all data frames into one using rbind
annotations_all <- do.call(rbind, data_list)

# Step 7: Move the "fileID" column to the first position
annotations_all <- annotations_all[, c("fileID", setdiff(names(annotations_all), "fileID"))]

# Step 8: Create a new column "duration" as the difference between End.Time..s. and Begin.Time..s.
annotations_all$duration <- annotations_all$End.Time..s. - annotations_all$Begin.Time..s.

# Step 5 Edit any irregular species codes to become "other"
# Replace any Species where the number of characters is not equal to four with "other"
annotations_all <- annotations_all %>%
  mutate(Species = ifelse(nchar(Species) != 4, "other", Species))

# Replace any occurrences of "?" in the "Species" column with "other"
annotations_all <- annotations_all %>%
  mutate(Species = ifelse(grepl("\\?", Species), "other", Species))

# Replace any lowercase letters in annotations_all$Species with uppercase letters
annotations_all$Species <- toupper(annotations_all$Species)

# Replace any instances of "OTHER" with "other" in the Species column
annotations_all$Species <- gsub("OTHER", "other", annotations_all$Species)

# Remove rows where Species is "REVI"
annotations_2 <- annotations_all %>%
  filter(Species != "REVI")

# Remove rows where Species is "other"
annotations_2 <- annotations_2 %>%
  filter(Species != "other")

annotations = annotations_2

# Add 'aberration' column to the far left with default values (modify as needed)
annotations <- annotations %>%
  mutate(aberration = sample(0:1, n(), replace = TRUE)) %>%  # Replace with your condition
  select(aberration, everything())  # Move 'aberration' to the first column

# Compute the median duration per species
median_durations <- annotations %>%
  group_by(Species) %>%
  summarise(median_duration = median(duration, na.rm = TRUE))

# Join the median durations back to the original data
annotations <- annotations %>%
  left_join(median_durations, by = "Species") %>%
  mutate(aberration = ifelse(duration > 2 * median_duration, 1, 0)) %>%
  select(aberration, everything(), -median_duration)  # Move 'aberration' to first column and remove median column

# Print the updated dataframe
head(annotations)

# Count total occurrences and aberrations per species
aberration_summary <- annotations %>%
  group_by(Species) %>%
  summarise(
    total_occurrences = n(),
    aberration_count = sum(aberration, na.rm = TRUE),
    aberration_frequency = aberration_count / total_occurrences
  )

# Print the summary
print(aberration_summary)


# Subset annotations where aberration == 1
aberrations <- annotations %>%
  filter(aberration == 1)

# Print the first few rows
head(aberrations)

# Count aberrations per fileID and species, add new columns
aberrations_by_fileID <- aberrations %>%
  group_by(fileID, Species) %>%
  summarise(
    count = n(),
    firstStart.sec = min(Begin.Time..s., na.rm = TRUE),
    firstDuration = duration[which.min(Begin.Time..s.)],  # Get duration for min Begin.Time..s.
    .groups = "drop"
  ) %>%
  filter(count > 0) %>%  # Keep only species with > 0 rows
  left_join(median_durations, by = "Species")  # Add median_duration column

# Print the updated data frame
print(aberrations_by_fileID)

# Sort by fileID and Species (if not already sorted)
aberrations_by_fileID <- aberrations_by_fileID %>%
  arrange(fileID, Species)

# Create a completely blank row with the same structure as aberrations_by_fileID
blank_row <- aberrations_by_fileID[1, ]  # Copy the structure
blank_row[,] <- NA  # Set all values to NA

# Identify where fileID changes
change_indices <- which(aberrations_by_fileID$fileID != lag(aberrations_by_fileID$fileID, default = first(aberrations_by_fileID$fileID)))

# Create an empty list to store rows
rows_list <- list()

# Loop through aberrations_by_fileID and insert blank rows where needed
for (i in seq_len(nrow(aberrations_by_fileID))) {
  if (i %in% change_indices) {
    rows_list <- append(rows_list, list(blank_row))  # Insert blank row before change
  }
  rows_list <- append(rows_list, list(aberrations_by_fileID[i, ]))  # Add the actual row
}

# Combine list into a new data frame
aberrations_to_save <- bind_rows(rows_list)

# Print the first few rows
print(aberrations_to_save)

# Write the data frame to an Excel file
write_xlsx(aberrations_to_save, "aberrations_out.xlsx")

######

unique_species <- unique(annotations$Species)
# Get the number of unique species
nSpecies <- length(unique_species)

# Loop through each species
for (s in 1:12) {
  species_name <- unique_species[s]
  print(paste("Processing species:", species_name))

# Define species of interest
spp_of_interest <- species_name  

# Filter annotations for the selected species
annotations_filtered <- annotations %>%
  filter(Species == spp_of_interest)

# Filter aberrations for the selected species
aberrations_filtered <- aberrations %>%
  filter(Species == spp_of_interest)

# Get max y-values for positioning annotations
max_y_plot1 <- max(ggplot_build(ggplot(annotations_filtered, aes(x = duration)) +
                                  geom_histogram(binwidth = 0.1))$data[[1]]$count)

max_y_plot2 <- max(ggplot_build(ggplot(aberrations_filtered, aes(x = duration)) +
                                  geom_histogram(binwidth = 0.1))$data[[1]]$count)

# Create histogram for annotations
plot1 <- ggplot(annotations_filtered, aes(x = duration)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(1, 10)) +  
  labs(y = "Frequency") +  
  theme_minimal() +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(),   # Hide x-axis values
        plot.margin = margin(0, 0, 0, 0)) +  # Remove extra white space
  annotate("text", x = 6, y = max_y_plot1 * 0.85, 
           label = spp_of_interest,
           hjust = 1, size = 4) +  
  annotate("text", x = 8.5, y = max_y_plot1 * 0.65,  
           label = "Duration of all annotations",
           hjust = 1, size = 3)  

# Create histogram for aberrations
plot_2 <- ggplot(aberrations_filtered, aes(x = duration)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(1, 10)) +  
  labs(x = "Duration (seconds)", y = "Frequency") +
  theme_minimal() +
  theme(plot.margin = margin(0, 0, 0, 0)) +  # Remove extra white space
  annotate("text", x = 3, y = max_y_plot2 * 0.6,  
           label = "Aberrations",
           hjust = 1, size = 3)  # Add annotation for aberrations

# Arrange plots vertically with optimized layout
final_plot <- plot1 / plot_2 + plot_layout(heights = c(1, 1))  # Equal height distribution

# Print the combined plot
print(final_plot)

# Save final_plot as a jpg
ggsave(filename = paste0("aberr_out/aberr_", spp_of_interest, ".jpg"),
       plot = final_plot,
       width = 3, height = 3, dpi = 300, device = "jpg")

}   # next species

#################
# List all JPG files in the folder
jpg_files <- list.files(path = image_folder, pattern = "\\.jpg$", full.names = TRUE)

# Check if any files exist
if (length(jpg_files) == 0) {
  stop("No JPG files found in the directory:", image_folder)
}

# Define layout: 2 columns x 3 rows (6 images per page)
cols <- 2
rows <- 3
images_per_page <- cols * rows

# Start PDF device
pdf(output_pdf, width = 8.5, height = 11)  # Landscape Letter size

# Process images in chunks of 4
for (i in seq(1, length(jpg_files), by = images_per_page)) {
  
  # Get a subset of up to 4 images
  img_subset <- jpg_files[i:min(i + images_per_page - 1, length(jpg_files))]
  
  # Create an empty grob list for images
  grob_list <- list()
  
  for (j in seq_along(img_subset)) {
    img <- tryCatch(readJPEG(img_subset[j]), error = function(e) NULL)
    
    if (!is.null(img)) {
      # Convert the image into a grob
      grob_list[[j]] <- rasterGrob(img, interpolate = TRUE)
    } else {
      grob_list[[j]] <- nullGrob()  # Placeholder if image fails
    }
  }
  
  # Create a grid layout with more padding
  grid.arrange(
    grobs = grob_list, 
    ncol = cols, 
    nrow = rows, 
    top = textGrob("Aberration Summary", gp = gpar(fontsize = 16, fontface = "bold")),
    padding = unit(3, "lines")  # Adds space around images
  )
}

# Close PDF device
dev.off()

