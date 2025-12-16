rm(list = ls())  # clear environment

library(jpeg)
library(grid)
library(gridExtra)
library(ggplot2)
library(pdftools)
library(magick)
library(dplyr)

####---------------------------------------------------------------------
# 05.Figs-to-pdf.v05.R
# Code for making one figure per day of "noise" per minute for each plot
# Input = location of folders of the form *_noise" with jpg files for one group
# Output = "*_noise_summary.pdf" in root directory
#
#   Warning. This code will delete any png files within your root directory
#
# Matt Ayres, 28 Dec 2024
####---------------------------------------------------------------------

# Set the directory where your folders of JPG files are located
workpath_root ="//dartfs-hpc/rc/lab/a/AyresM/Root/birdSound_HBEF/2023"
setwd(workpath_root)

# List all directories matching the pattern "*_noise"
group <- list.dirs(full.names = FALSE) %>%
  grep(pattern = "_noise$", value = TRUE)

# Print the result
group

# Loop over each element in groups
for (g in 1:length(group)) {
  jpg_directory = (paste0(workpath_root,"/",group[g]))
  output_png = paste0(workpath_root,"/",group[g],"/",group[g])
  output_pdf = paste0("noiseSummary_",group[g],".pdf")
  groupName=group[g]

# List JPG files in the directory
jpg_files <- list.files(jpg_directory, pattern = ".jpg", full.names = TRUE)
jpg_images <- lapply(jpg_files, readJPEG)

pdf(output_pdf, width = 11, height = 8.5)  # Landscape mode, puts it in root directory
num_pages <- ceiling(length(jpg_images) / 4)

for (i in 1:num_pages) {
  start_index <- (i - 1) * 4 + 1
  end_index <- min(length(jpg_images), i * 4)
  
  # Subset images for the current page
  page_images <- jpg_images[start_index:end_index]
  
  # Arrange images into a grid
  grid.arrange(grobs = lapply(page_images, rasterGrob), ncol = 2, nrow = 2)
}

# Close all open graphics devices
while (!is.null(dev.list())) {
  dev.off()
}

#Add groupName as header to all pdf pages
  # Convert PDF to images
  pages <- pdf_convert(output_pdf, format = "png", dpi = 300)

  # Function to add header to image
  add_header_to_image <- function(image_path, text) {
    image <- image_read(image_path)
    image <- image_annotate(image, text, size = 100, gravity = "northeast", location = "+10+10", font = "Helvetica")
    return(image)
  }

  # Add header to each image
  images_with_headers <- lapply(pages, add_header_to_image, text = groupName)

  # Combine images into a single PDF
  # pdf_path <- paste0(workpath_root,
  image_write(image_join(images_with_headers), output_pdf, format = "pdf")

# Close all open graphics devices
while (!is.null(dev.list())) {
  dev.off()
}

  # delete the temporary png files that were created
  # List all .png files in the current directory
  png_files <- list.files(pattern = "\\.png$")
  file.remove(png_files)
  
} ### next group
  
