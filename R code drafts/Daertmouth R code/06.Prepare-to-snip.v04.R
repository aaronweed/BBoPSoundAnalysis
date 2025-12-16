

# ...................................
# 06.Prepare-to-snip.v04.R
# Matt Ayres, 30 Dec 2024
# Prepare file for snipping selected segments from targeted sound files
# Input1 = soundManifest_HB_2024.v01.xlsx (full manifest)
# Input2 = OVEN Breeding Stage Sounds to Extract 2024.xlsx (list of plots x dates to extract)
# Input3 = Sunrise&Sunset_HBEF.v05.xlsx
# Output = Sounds-to-extract.2023-11-30.xlsx (matches format expected by )

#....................................
rm(list = ls())     # clear environment
library(exifr)
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(digest)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(tuneR)
library(lubridate)

workpath_root="F:/HB_2024_SoundFiles/2024"   # to root directory for putput of targets manifest
fileBase = 1000   # base number for fileID

snipDir = "F:/HB_2024_SoundFiles/2024/snipsOut.2024-12-30"
inFile1 = "F:/HB_2024_SoundFiles/2024/soundManifest_HB_2024.v01.xlsx"
  inSheet1 = "manifest"
inFile2 = "F:/HB_2024_SoundFiles/2024/soundManifest_HB_2024.v01.xlsx"
  inSheet2 = "targets"
inFile3 = "F:/HB_2024_SoundFiles/2024/Sunrise&Sunset_HBEF.v05.xlsx"
  inSheet3 = "daylength"
outFile1 = "Sounds-to-extract.2024-12-30.xlsx"
  outSheet1 = "targets"
  
timeStart = 5      # Target sound files start at 0500
startTimeInitial = 500    # start time as hhmm
GMT_base = -4           # Time zone of study area in sunrise data (= standard time)
GMT_targets = -5        # Time zone of study area during recordings (5 in NH if daylight savings time)

setwd(workpath_root)

# Open sheet inFile1, inSheet1 as df1_full
df1_full <- read_excel(inFile1, sheet = inSheet1)

# Open sheet inFile2, inSheet2 as df2_targets
df2_targets <- read_excel(inFile2, sheet = inSheet2)

# Open sheet inFile3, inSheet2 as df3_sunrise
df3_sunrise <- read_excel(inFile3, sheet = inSheet3)

# Print to confirm
head(df1_full)
head(df2_targets)
head(df3_sunrise)

# Create a new column in targets with mmdd
df2_targets <- df2_targets %>%
  mutate(
    # Ensure Date is in Date format
    Date = as.Date(Date),
    
    # Extract month and day, format as mmdd and convert to numeric
    mmdd = as.numeric(sprintf("%02d%02d", month(Date), day(Date)))
  )

# delete all rows in full manifest except for 0500
# Filter rows where startTime.hhmm is 500
df1_full <- df1_full %>%
  filter(startTime.hhmm == startTimeInitial)

# Print to confirm
head(df1_full)

# Perform the merge (left join to keep all rows from df2_targets)
df3_merge <- df2_targets %>%
  left_join(df1_full, by = c("plot", "mmdd" = "date.mmdd"))

# Check for multiple matches
multiple_matches <- df1_full %>%
  inner_join(df2_targets, by = c("plot", "date.mmdd" = "mmdd")) %>%
  group_by(plot, date.mmdd) %>%  # Use date.mmdd here
  filter(n() > 1) %>%
  ungroup()

# Check for no matches (NA in columns from df1_full)
no_matches <- df3_merge %>%
  filter(is.na(plot))  # Or another column from df1_full

# Alerts for multiple or no matches
if (nrow(multiple_matches) > 0) {
  warning("Multiple matches found for some rows in df1_full.")
}

if (nrow(no_matches) > 0) {
  warning("No matching rows found for some rows in df2_targets.")
}

# Print to confirm
head(df3_merge)

# Select and reorder columns from df3_merge
df4_out <- df3_merge %>%
  select(path, file, subsequent.file1, year, area, group, plot, mmdd)

# Print to confirm
head(df4_out)

# Add two new columns to df4_out
df4_out <- df4_out %>%
  mutate(
    targetStart.min = NA,  # Initialize with NA or any default value
    random = NA            # Initialize with NA or any default value
  )

# Print to confirm
head(df4_out)

########################## process sunrise data

df4_sunrise <- df3_sunrise %>%
  mutate(sunrise.hhmm.GMT4 = str_pad(as.character(sunrise.hhmm.GMT4), 
                                     width = 4, 
                                     pad = "0"))

# Convert hhmm to fraction of the day
df4_sunrise <- df4_sunrise %>%
  mutate(
    # Extract hours and minutes from hhmm format
    hour = as.numeric(substr(sunrise.hhmm.GMT4, 1, 2)),
    minute = as.numeric(substr(sunrise.hhmm.GMT4, 3, 4)),
    
    # Calculate fraction of the day
    sunrise_fraction = (hour * 60 + minute) / (24 * 60)
  ) %>%
  # Optionally, remove intermediate columns
  select(-hour, -minute)

  # Print to confirm
  hist(df4_sunrise$sunrise_fraction)

# Adjust for daylight savings time 
df4_sunrise$sunrise_fraction_local = df4_sunrise$sunrise_fraction+(GMT_base-GMT_targets)/24
  # Print to confirm
  hist(df4_sunrise$sunrise_fraction_local)

# Calculate fraction of day that has elapsed at 40 minutes after sunrise
df4_sunrise <- df4_sunrise %>%
    mutate(birdTime_fraction = sunrise_fraction_local + 40 / (24 * 60))
    hist(df4_sunrise$birdTime_fraction)
  
# For each day, calculate hhmm when it is 40 minutes after sunrise
    df4_sunrise <- df4_sunrise %>%
      mutate(
        # Calculate hours and minutes from the fraction of the day
        hour = floor(birdTime_fraction * 24),
        minute = round((birdTime_fraction * 24 * 60) %% 60),
        
        # Format as hhmm with leading zeros
        birdTime_hhmm = str_pad(hour, 2, pad = "0") %>%
          paste0(str_pad(minute, 2, pad = "0"))
      ) %>%
      # Optionally remove intermediate columns
      select(-hour, -minute)
    
    # For each day, calculate minutes after 0500 when it is 40 minutes after sunrise   
    df4_sunrise <- df4_sunrise %>%
      mutate(
        min_after_5 = round((birdTime_fraction - timeStart / 24) * 24 * 60)
      )

    # Add a column to sunrise data frame with mmdd    
    df4_sunrise <- df4_sunrise %>%
      mutate(
        # Convert Excel date to R Date format
        Date = as.Date(Date.excel, origin = "1899-12-30"),
        
        # Extract month and day, format as mmdd and convert to numeric
        mmdd = as.numeric(sprintf("%02d%02d", month(Date), day(Date)))
      )

    # Perform the left join to bring min_after_5 into df4_out
    df4_out <- df4_out %>%
      left_join(
        df4_sunrise %>%
          select(mmdd, min_after_5) %>%
          group_by(mmdd) %>%
          summarise(min_after_5 = first(min_after_5), .groups = "drop"), # Handles multiple matches by taking the first
        by = "mmdd"
      ) %>%
      mutate(targetStart.min = min_after_5) %>%
      select(-min_after_5)
    
    # Check for multiple matches in df4_sunrise
    multiple_matches2 <- df4_sunrise %>%
      group_by(mmdd) %>%
      filter(n() > 1) %>%
      ungroup()
    
    # Check for no matches in df4_out
    no_matches2 <- df4_out %>%
      filter(is.na(targetStart.min))
    
    # Alerts for multiple or no matches
    if (nrow(multiple_matches) > 0) {
      warning("Multiple matches found for some rows in df4_sunrise.")
    }
    
    if (nrow(no_matches) > 0) {
      warning("No matching rows found for some rows in df4_out.")
    }
 
    # random variable for sorting
    df4_out <- df4_out %>%
      mutate(random = rnorm(n(), mean = 0, sd = 1))   
    
    #sort
    df4_out <- df4_out %>%
      arrange(random)
  
    # Create file ID
    df4_out <- df4_out %>%
      mutate(fileID = fileBase + row_number()) %>%
      relocate(fileID, .before = everything())  # Move fileID to the far left  

    # Write df4_out to Excel with the specified sheet name
    write_xlsx(setNames(list(df4_out), outSheet1), path = outFile1)  
    
    