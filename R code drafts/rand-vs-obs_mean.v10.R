rm(list = ls())     # clear environment

# Load necessary library
library(dplyr)
library(exifr)
library(tidyverse)
library(writexl)
library(tidyr)
library(readxl)
library(plotly)

# ...............................................
# rand-vs-obs_mean.v10.R
# Starts with the long list of individual annotations
#
# 20 Oct 2024 (19 April 2025)
#
#................................................

workpath_root = 'C:/Ayres/Ayres.Research/BBoP/2025_04.MABI'   # to root directory
inFile1 = "MABI.annotations.n87700.xlsx"
inSheet1 = "data"
outFolder = "random/mean/"
inFile2 = "random/RandomOut.mean.thresholds.v02.xlsx"
inSheet2 = "preR"
fileOut2 = "rand_obs_long_mean.xlsx"
testStatistic_max = 50  # for graphing points off the graph

# Set threshold for separating cases with countersinging
min_time_to_next = 5    # sec

# Set threshold where the first vocalization is too late
lastCall = 540    # sec

setwd(workpath_root)

#### .......................................
#### Read in thresholds from randomizations
preR_data <- read_excel(inFile2, sheet = inSheet2)

# View the first few rows of the dataframe
head(preR_data)

# Ensure all columns except the first are numeric
preR_data_mean <- preR_data
preR_data_mean[, -1] <- lapply(preR_data[, -1], function(col) as.numeric(as.character(col)))

# View the first few rows of the new dataframe
head(preR_data_mean)

inData = preR_data_mean

##### .......................

annotations_all <- read_excel(inFile1)

  # Move  "fileID"  to the first column
  annotations_all <- annotations_all[, c("fileID", setdiff(names(annotations_all), "fileID"))]
  # View the first few rows of the new dataframe
  head(annotations_all)
  
  # Create a new column "duration" as the difference between End.Time..s. and Begin.Time..s.
  annotations_all$duration <- annotations_all$End.Time..s. - annotations_all$Begin.Time..s.

  annotations = annotations_all
  
  unique_species <- sort(unique(annotations$Species))
  print(unique_species)

# Create a frequency table of the "Species" column
species_counts <- table(annotations$Species)

# Convert the table into a data frame
species_counts_df <- as.data.frame(species_counts)

  # Rename the columns for better clarity
  colnames(species_counts_df) <- c("Species", "Count")

  # Sort species_counts_df by Count in descending order
  species_counts_df <- species_counts_df[order(-species_counts_df$Count), ]
  
  # Remove the row where Species equals "REVI", EAPH, RESQ, EACH, or UNMA
  species_counts_df <- species_counts_df[species_counts_df$Species != "REVI", ]
  species_counts_df <- species_counts_df[species_counts_df$Species != "RESQ", ]
  species_counts_df <- species_counts_df[species_counts_df$Species != "EACH", ]
  species_counts_df <- species_counts_df[species_counts_df$Species != "UNMA", ]
  species_counts_df <- species_counts_df[species_counts_df$Species != "EAPH", ]  
  # Leaves 75 species in MABI data
    
  # Remove species where total of annotated vocalizations is < 10
  species_counts_df <- species_counts_df[species_counts_df$Count >= 10, ]
  # Leaves 57 species in MABI data
   
  # View the sorted data frame
  head(species_counts_df)
  
# Repeat the code below as a loop repeated for the length of species_counts_df 
  # At the start of each loop, define sppName1 as the corresponding value for 
  #  species_counts_df$Species
  
  # Initialize an empty data frame to store results summary
  rand_vs_obs_summary <- data.frame(
    sppName1 = character(),
    num_singers = integer(),
    count_p0.05 = integer(),
    count_p0.01 = integer(),
    count_p0.95 = integer(),
    stringsAsFactors = FALSE
  )  

  # Initialize an empty data frame to store results in long form
  rand_vs_obs_long <- data.frame(
    sppName1 = character(),
    num_singers = integer(),
    mean.time_to_next = numeric(),  # Corrected to numeric()
    SD.time_to_next = numeric(),    # Corrected to numeric()
    p0.01 = integer(),
    p0.05 = integer(),
    p0.95 = integer(),
    stringsAsFactors = FALSE
  )
  
#### Repeat the loop below for each species 
# Loop over the length of species_counts_df
for (i in seq_len(nrow(species_counts_df))) {

  # Extract sppName1 as a character string
  sppName1 <- as.character(species_counts_df$Species[i])
  
  # Print sppName1 to verify
  print(sppName1)

# Go on to analyses of intervals between songs
# Create a new dataframe that contains only rows where Species is as specified

focalSpp <- subset(annotations_all, Species == sppName1)
hist(focalSpp$duration)

# Sort focalSpp by fileID and Begin.Time..s
focalSpp <- focalSpp[order(focalSpp$fileID, focalSpp$Begin.Time..s.), ]

### Remove any cases where the first vocalization is too late (> lastCall)
# Identify fileIDs where the minimum Begin.Time..s. is < lastCall
fileIDs_to_remove <- focalSpp %>%
  group_by(fileID) %>%
  summarise(min_begin_time = min(Begin.Time..s., na.rm = TRUE)) %>%
  filter(min_begin_time > lastCall) %>%
  pull(fileID)

# Filter out those fileIDs from focalSpp
focalSpp_cleaned <- focalSpp %>%
  filter(!(fileID %in% fileIDs_to_remove))

# Calculate the time difference to the next row for each fileID
focalSpp$time_to_next <- c(diff(focalSpp$Begin.Time..s.), NA)

# Calculate the difference between consecutive elements in "time_to_next" as real numbers
focalSpp$time_to_next <- c(diff(focalSpp$Begin.Time..s.), NA)  # Add NA for the last row as there's no next value

# Loop through each row, except the last one, and set time_to_next to NA if fileID[r+1] != fileID[r]
focalSpp$time_to_next[which(focalSpp$fileID[-nrow(focalSpp)] != focalSpp$fileID[-1])] <- NA

# Count unique fileID cases in focalSpp
num_unique_fileID <- focalSpp %>%
  summarize(n = n_distinct(fileID)) %>%
  pull(n)

# Print the result
print(num_unique_fileID)

    # Filter time_to_next values to be between 0 and 20
    time_to_next_filtered <- focalSpp$time_to_next[focalSpp$time_to_next <= 20]

    # Create breaks to include the full range of data
    breaks <- seq(0, 20, by = 1)

    # Adjust the upper limit of breaks slightly to ensure inclusion of all values
    breaks[length(breaks)] <- max(breaks[length(breaks)], max(time_to_next_filtered, na.rm = TRUE))

    # Plot the histogram
    hist(time_to_next_filtered, 
     breaks = breaks, 
     main = sppName1, 
     xlab = "Time to next song (seconds)", 
     xlim = c(0, 20),  # x-axis limits from 0 to 20
     col = "lightblue", 
     border = "black")

# Create a new data frame, “df_culled1” in which all rows of focalSpp for any given 
# focalSpp$fileID have been deleted whenever any case of 
#    focalSpp$time_to_next is less than min_time_to_next. 

# Step 1: Identify fileIDs with any time_to_next value less than min_time_to_next5

fileIDs_to_exclude <- focalSpp %>%
  filter(time_to_next < min_time_to_next) %>%
  pull(fileID) %>%
  unique()

# Step 2: Create a new dataframe with rows where fileID is in fileIDs_to_exclude
culls_counterSinging <- focalSpp %>%
  filter(fileID %in% fileIDs_to_exclude)

# View the resulting dataframe
head(culls_counterSinging)

# Create a list of unique fileIDs from culls_counterSinging
counterSinging_fileIDs <- culls_counterSinging %>%
  pull(fileID) %>%
  unique()

# View the list of unique fileIDs
print(counterSinging_fileIDs)

# Step 2: Filter out all rows from focalSpp where fileID is in fileIDs_to_exclude
df_culled1 <- focalSpp %>%
  filter(!fileID %in% fileIDs_to_exclude)

# Count unique fileID cases in df_culled1
num_unique_fileID <- df_culled1 %>%
  summarize(n = n_distinct(fileID)) %>%
  pull(n)

# Print the result
print(num_unique_fileID)

# graph the histogram with countersinging removed
# Filter time_to_next values to be between 0 and 20
time_to_next_filtered <- df_culled1$time_to_next[df_culled1$time_to_next <= 30]

hist(time_to_next_filtered, 
     breaks = seq(0, 30, by = 1), 
     main = sppName1, 
     xlab = "Time to next song (seconds)", 
     xlim = c(0, 30),  # x-axis limits from 0 to 20
     col = "lightblue", 
     border = "black")

### no removal of files with first song too early or late

df1=df_culled1

# Calculate number of data points and mean for each fileID
summary_stats <- df1 %>%
  group_by(fileID) %>%  # Group by fileID
  summarize(
    nSongs = n(),                          # Number of data points
    time_to_next_SD = sd(time_to_next, na.rm = TRUE),    # SD of time_to_next
    time_to_next_mean = mean(time_to_next, na.rm = TRUE),    # mean of time_to_next    
    time_to_next_median = median(time_to_next, na.rm = TRUE)    # median of time_to_next
  ) %>%
  ungroup()  # Ensure fileID is a regular column, not a grouping variable

# View the resulting data frame
print(summary_stats)

# Add sppName1 as the first column
summary_stats <- summary_stats %>%
  mutate(sppName1 = sppName1) %>%  # Add sppName column
  select(sppName1, everything())  # Reorder columns to place sppName at the far left


# View the updated data frame
print(summary_stats)


##### ......... Create graph

# Name the output file
outPlot=paste0(outFolder,sppName1,".png")

# Open a PNG device with specified dimensions and resolution
png(filename = outPlot, width = 7, height = 9, units = "in", res = 300)
  # Adjust margins to prevent axis label cropping (bottom, left, top, right)
  par(mar = c(5, 6, 4, 2))

# Create the base plot for the first spline
plot(
  inData$nSongs,
  inData$p0.05,
  type = "n",  # No points or lines initially
  xlab = "Number of vocalizations",
  ylab = "Mean time to next vocalization",
  ylim = c(0, testStatistic_max),  # Set y-axis limits
  xlim = c(0, max(inData$nSongs)),  # Set x-axis limits
  main = "",
  cex.lab = 2,  # Double font size for axis legends
  cex.axis = 1.5  # Increase font size for tick labels
)

# Label the functions showing lower limits of random distributions
text(x = 15, y = 40, labels = "p < 0.05", adj = c(0, 0), cex = 1.5)  # Lower left corner at (30, 40)
text(x = 25, y = 30, labels = "p < 0.01", adj = c(0, 0), cex = 1.5)  # Lower left corner at (40, 30)


# Add title at the top using sppName1
title(main = sppName1, cex.main = 2.5)  # Large font for the title

# Fit and plot a smoother spline for minVar_0.05
lines(
  spline(inData$nSongs, inData$p0.05, n = 30), # Smoother spline with n points
  col = "black", 
  lwd = 1
)

# Fit and plot a smoother spline for minVar_0.01
lines(
  spline(inData$nSongs, inData$p0.01, n = 30), # Smoother spline with n points
  col = "black", 
  lwd = 1
)

# Identify cases where testStatistic > testStatistic_max
high_mean <- summary_stats$time_to_next_mean > testStatistic_max & !is.na(summary_stats$time_to_next_mean)

# Add jittered points for mean > 50 (if any exist), with reduced y-axis jitter
if (any(high_mean)) {
  points(
    jitter(summary_stats$nSongs[high_mean], amount = 0.5),  # Jitter x-axis values
    jitter(rep(testStatistic_max, sum(high_mean, na.rm = TRUE)), amount = 1),  # Reduced jitter for y-axis
    pch = 17,  # Triangle symbol
    col = "blue"  # Use blue color for high-mean indicators
  )
}

# Overlay datapoints for mean <= 50
points(
  summary_stats$nSongs[!high_mean], 
  summary_stats$time_to_next_mean[!high_mean], 
  pch = 16,  # Solid circles for points
  col = "red"  # Use red color for standard points
)

# Close the PNG device
dev.off()

############ score each fileID for deviations from random


# Initialize the new columns with NA values
rand_vs_obs_temp <- summary_stats %>%
  mutate(
    p0.01 = NA,  # Add p0.01 column
    p0.05 = NA,  # Add p0.05 column
    p0.95 = NA   # Add p0.95 column
  )

# Remove rows where testStat is NA
rand_vs_obs_temp <- rand_vs_obs_temp %>%
  filter(!is.na(time_to_next_mean))

# Remove rows where testStat is NA
rand_vs_obs_temp <- rand_vs_obs_temp %>%
  filter(!is.na(time_to_next_mean))

# View the resulting data frame
print(rand_vs_obs_temp)

# Remove rows where nSongs < 3
rand_vs_obs_temp <- rand_vs_obs_temp %>%
  filter(nSongs >= 3)

# View the updated data frame
print(rand_vs_obs_temp)

nFiles = nrow(rand_vs_obs_temp)

for (f in 1:nFiles) {
  testStat_temp = rand_vs_obs_temp$time_to_next_mean[f]

  # Find the first row in inData where nSongs matches rand_vs_obs_temp$nSongs[f]
  r <- which(inData$nSongs == rand_vs_obs_temp$nSongs[f])[1]
  
  # Print the result
  print(r)
  
  # retrieve the threshold values for p0.01, p0.05, p0.95
  thresh_p0.01 = inData$p0.01[r]
  thresh_p0.05 = inData$p0.05[r]
  thresh_p0.95 = inData$p0.95[r]
  
  if (exists("testStat_temp") && 
      !is.null(testStat_temp) && 
      is.numeric(testStat_temp) && 
      !is.na(testStat_temp) && 
      testStat_temp > 0) {
    
    if (testStat_temp < thresh_p0.01) {
      rand_vs_obs_temp$p0.01[f] <- 1  # Assign 1 if the condition is true
    } else {
      rand_vs_obs_temp$p0.01[f] <- 0  # Assign 0 otherwise
    }
    
    if (testStat_temp < thresh_p0.05) {
      rand_vs_obs_temp$p0.05[f] <- 1  # Assign 1 if the condition is true
    } else {
      rand_vs_obs_temp$p0.05[f] <- 0  # Assign 0 otherwise
    }
    
    if (testStat_temp > thresh_p0.95) {
      rand_vs_obs_temp$p0.95[f] <- 1  # Assign 1 if the condition is true
    } else {
      rand_vs_obs_temp$p0.95[f] <- 0  # Assign 0 otherwise
    }
  }
}     # next file within this species

# Append rand_vs_obs_temp to rand_vs_obs_long
rand_vs_obs_long <- rbind(rand_vs_obs_long, rand_vs_obs_temp)
}     # next species

  
  # Save the data frame as an Excel file
  write_xlsx(rand_vs_obs_long, fileOut2)
  
  # Confirmation message
  cat("Data frame saved as fileOut2 \n")
  
  