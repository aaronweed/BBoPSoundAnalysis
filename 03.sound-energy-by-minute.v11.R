

####---------------------------------------------------------------------
# 03.sound-energy-by-minute.v11.R
# Code for reading a manifest, where each row contains path to 3 subsequent sound files
# Calculates SD of sound pressure for each minute in each series of sound files
# Saves log10(SD) as "log10RMSE" as a measure of background sound pressure ("noise")
# Saves one jpg for each recorder on each day of log10(RMSE) per minute
# Provides input for code that creates one composite figure for day of all plots
# Modified to include startTim_hhmm in output csv
#
# Matt Ayres, 12 March 2024 (12 Jan 2025)
####---------------------------------------------------------------------

rm(list = ls())     # clear environment
library(tuneR)
library(zoo)        # for moving averages
library(readxl)
library(openxlsx)
library(audio)
library(ggplot2)
library(seewave)
library(dplyr)
# library(sf)
# This code invokes "sox", which must be included in the system variable "PATH" in Windows


workpath_root="//dartfs-hpc/rc/lab/a/AyresM/Root/audio.process/BBoP.2022"   # to directory with file manifest
workpath_flac_root = "K:/Raw_Audio_File/2022"   # directory prefix for "path" and "file" in file manifest
workpath_out = "//dartfs-hpc/rc/lab/a/AyresM/Root/audio.process/BBoP.2022"     # directory for output files
workpath_temp = "E:/Ayres/audio.process"      # directory for temporarily writing and reading wav files saved from flac

manifest_fileName="soundManifest_BBoP_2022.v01.xlsx"
manifest_sheet="soundManifest"

outFile_csv="BBoP_2022.noiseByMinute.csv"

yLow = 2.3        # standard low for y axis
yHigh = 4.0       # standard high for y axis
# yLow = 1.3        #  low for y axis for 2020
# yHigh = 3.0       #  high for y axis for 2020

# ................ end user adjustable parameters.................

setwd(workpath_root)
data_dir_root=workpath_flac_root
outpath=workpath_out
outpath_jpg=file.path(outpath, "noise_jpg_all") 

# Create output subdirectories if they do not exist
if (!file.exists(outpath)) {
  dir.create(outpath)
}

if (!file.exists(outpath_jpg)) {
  dir.create(outpath_jpg)
}

preManifest=read.xlsx(manifest_fileName, manifest_sheet)

# Filter manifest for rows where startTime equals 500
manifest <- preManifest %>%
  filter(startTime.hhmm == 500)
  # Also drop if fileLength_min <> 59-61
  # for now, I just did this manually before reading the file

fmax = nrow(manifest)
nRecorders = length(unique(manifest$plot))
nDays = length(unique(manifest$date.mmdd))
nMinutes = 180
nrow_out=nMinutes*nRecorders*nDays

ffirst=1
flast=fmax

for (f in ffirst:flast) {   # for each row of input manifest, each with 2 subsequent sound files
  # I was unable to get R to read flac files directly, so used sox to rewrite each flac as a wav file  
   # For this to work, sox must be installed to the system of the computer running this code
  if (exists("audio1")) {
    rm(audio1)
    rm(audio2)
    rm(audio3)
  }
  
  # delete any previous versions of temporary wav files in this directory
  files_to_delete <- list.files(workpath_temp, pattern = "^output_file_.*\\.wav$", full.names = TRUE)
  file.remove(files_to_delete)
    
  # Check if the loop index is a multiple of 100
  if (f %% 100 == 0) {
    cat("File index:", f, "\n")
  }

   flac_file = file.path(data_dir_root, manifest$path[f], manifest$file[f])
      flac_file = gsub("//", "/", flac_file)
      
    # Assign wav_file within the workpath_temp directory
      wav_file <- file.path(workpath_temp, "output_file_1.wav") # Specify the output for temp wav file
      
    # write wav file and then read it
    system(paste("sox", shQuote(flac_file), shQuote(wav_file)))  

  audio1 = readWave(wav_file)

  if (!is.na(manifest$subsequent.file1[f])) {
      flac_file = file.path(data_dir_root, manifest$path[f], manifest$subsequent.file1[f])
        flac_file = gsub("//", "/", flac_file)
        wav_file <- file.path(workpath_temp, "output_file_2.wav") # Specify the output for temp wav file
    system(paste("sox", shQuote(flac_file), shQuote(wav_file)))  
    audio2 = readWave(wav_file)
  }

  if (!is.na(manifest$subsequent.file2[f])) {    
    flac_file = file.path(data_dir_root, manifest$path[f], manifest$subsequent.file2[f])
     flac_file = gsub("//", "/", flac_file)
     wav_file <- file.path(workpath_temp, "output_file_3.wav") # Specify the output for temp wav file
    system(paste("sox", shQuote(flac_file), shQuote(wav_file)))  
    audio3 = readWave(wav_file)
  }
  
  audioFull <- audio1@left
  # Check if audio2 exists
  if (exists("audio2")) {
    # Perform the append operation
    audioFull <- append(audio1@left, audio2@left)
    if (exists("audio3")) {
      # Perform the append operation
      audioFull <- append(audioFull, audio3@left)
    }
  }
  
  sample_rate = audio1@samp.rate
  duration_minutes = length(audioFull) / sample_rate / 60
  minuteMax = floor(duration_minutes)     # truncate any data after the last full minute

log10RMSE = array(dim = c(minuteMax, 1)) 
noise_out2 <- matrix(nrow = 180, ncol = 9)
colnames(noise_out2) = c("area", "year", "group", "date", "plot", "startTime.hhmm", "minute", "log10RMSE", "movAvg11")
nRow=0
for (m in 1:minuteMax) {      # skip first minute
    nRow=nRow+1
    start_sample = (m) * 60 * sample_rate + 1
    end_sample = (m+1) * 60 * sample_rate
  selection = audioFull[start_sample:end_sample]
  log10RMSE[m] = log10(sd(selection))

  noise_out2[nRow,1] = manifest$area[f]
  noise_out2[nRow,2] = manifest$year[f]
  noise_out2[nRow,3] = manifest$group[f]  
  noise_out2[nRow,4] = manifest$date.mmdd[f] 
  noise_out2[nRow,5] = manifest$plot[f]
  noise_out2[nRow,6] = manifest$startTime.hhmm[f]
  noise_out2[nRow,7] = m
  noise_out2[nRow,8] = log10RMSE[m]
}

# Subset noise_out2 to remove rows with NA in the "area" column
# Find the column index of 'area' column
area_col_index <- which(colnames(noise_out2) == "area")
# Remove rows with NA in 'area' column
noise_out2 <- noise_out2[!is.na(noise_out2[, area_col_index]), ]
  num_rows <- nrow(noise_out2)

  moving_average = rollmean(log10RMSE, k = 11, fill = NA) # moving average 
    noise_out2[,9] = moving_average

# Make graphs
  noise_plot = data.frame(row_number = seq_along(log10RMSE), values = log10RMSE)
  colnames(noise_plot) <- c("minute", "log10RMSE")

  moving_plot = data.frame(row_number = seq_along(moving_average), values = moving_average)
  colnames(moving_plot) <- c("minute", "RollingAverage")

  seriesName = paste(manifest$area[f], "-", manifest$group[f], "-", manifest$plot[f], ".",  
                     manifest$year[f], "-", manifest$date.mmdd[f], sep = "")
  plotName = paste(manifest$area[f], "-", manifest$group[f], "-", manifest$plot[f], ".",
                   manifest$year[f], "-",manifest$date.mmdd[f], ".jpg", sep = "")

  if (length(dev.list()) > 0) {
    dev.off()  # Close the graphics device if open
  }
if (nrow(noise_plot) > 10) {
  plot <- ggplot() +
    geom_line(data = noise_plot, aes(x = minute, y = log10RMSE, color = "byMinute")) +
    geom_line(data = moving_plot, aes(x = minute, y = RollingAverage, color = "movAvg11")) +
    labs(x = "minutes", y = "log10RMSE", title = seriesName) +
    scale_color_manual(name = "Lines", values = c("blue", "red")) + 
    ylim(yLow, yHigh)  # scale y axis
    print(plot)
  outFile = file.path(outpath_jpg, plotName) 

  ggsave(outFile, plot, width = 6, height = 4, units = "in", dpi = 300)
}

# append data from the recorder just analyzed to growing data frame   
  noise_out3 <- as.data.frame(noise_out2)   # convert to proper data frame
 
  # create empty data frame for first file 
  if (f == ffirst) {
    noise_out4 <- data.frame(
    area = character(0),
    year = integer(0),
    group = character(0),
    date = as.Date(character(0)),
    plot = character(0),
    startTime.hhmm = integer(0),
    minute = integer(0),
    log10RMSE = numeric(0),
    movAvg11 = numeric(0),
    stringsAsFactors = FALSE
    )
  }

  noise_out4 = rbind(noise_out4, noise_out3)
}


# Split the data frame by the 'group' and 'year' variable
interaction_var <- interaction(noise_out4$group, noise_out4$year)
grouped_data <- split(noise_out4, interaction_var)

# Iterate over the list and write each group to a separate CSV file
for(interaction_name in names(grouped_data)) {
  group_df <- grouped_data[[interaction_name]]
  file_name <- paste0("group_", interaction_name, ".csv")
  write.csv(group_df, file_name, row.names = FALSE)
}



