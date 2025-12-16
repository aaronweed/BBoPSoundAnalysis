
####---------------------------------------------------------------------
# 04.makeGraphs.v07.R
# Code for making one figure per day of "noise" per minute (including all plots)
# Input = output of 03.sound-energy-by-minute.vNN.R
# Input = files matching "group_*.csv" in the current directory
#       = log10(RMSE) for each minute of each recorder in the original file manifest
# Output = one figure per day of sound energy per minute (for each plot, and for average) 
# Matt Ayres, 28 Dec 2024
####---------------------------------------------------------------------


rm(list = ls())  # clear environment
library(readxl)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(stringr)

workpath_root ="//dartfs-hpc/rc/lab/a/AyresM/Root/birdSound_HBEF/2023"
setwd(workpath_root)

# Find all files matching "group_*.csv" in the current directory
inFile <- list.files(pattern = "^group_.*\\.csv$")

# Extract the text between "group_" and ".csv"
group <- str_extract(inFile, "(?<=group_).*(?=\\.csv)")


# Print the result
group

# Loop over each element in inFile
for (g in 1:length(inFile)) {
  setwd(workpath_root)
  
groupName=group[g]
yLow = 2.3        # standard low for y axis
yHigh = 4.0       # standard high for y axis
# yLow = 1.3        #  low for y axis for 2020 and 2021
# yHigh = 3.0       #  high for y axis for 2020 and 2021

  input_fileName = inFile[g]
  outPath=paste0(groupName, "_noise")

data <- read.csv(input_fileName)
  unique_dates <- unique(data$date)
  num_dates <- length(unique_dates)

# Create the "outpath" subdirectory if it doesn't exist
if (!file.exists(outPath)) {
  dir.create(outPath)
}
setwd(outPath)

# Loop through each unique date
for (i in seq_along(unique_dates)) {
  # Subset data for the current date
  date <- unique_dates[i]
  subset_byDate <- data[data$date == date, ]
  
  # Calculate the average movingAverage.10 for each minute across all plots
  average_data <- subset_byDate %>%
    group_by(minute) %>%
    summarise(avg_movAvg11 = mean(movAvg11, na.rm = TRUE))
  
  # Create the line graph for the current date
  graphTitle <- paste(groupName, subset_byDate$date[1], subset_byDate$year[1])
  graphName = paste0(groupName, "_", subset_byDate$date[1], '_', subset_byDate$year[1], '.jpg')
  p <- ggplot() +
    geom_line(data = average_data, aes(x = minute, y = avg_movAvg11), 
              color = "red", size = 1.5)  +  # Red line for the average
    geom_line(data = subset_byDate, aes(x = minute, y = movAvg11, group = plot),
              color = "blue", size = 0.25) +  # Blue line for each plot
    labs(x = "Minute", y = "Moving average of sound pressure", title = graphTitle) +  # Add title
    ylim(yLow, yHigh) +  # Set the limits of the y-axis
    xlim(0, 180) +
    theme_minimal()
  
  # Print the plot
  print(p)
  
  # save to a jpg
  ggsave(graphName, plot = p, width = 6, height = 4, units = "in", dpi = 300)
  
}

}     # next group

