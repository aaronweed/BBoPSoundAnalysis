

# ...................................
# 07.Snip.v04.R
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
library(seewave)
library(openxlsx)


workpath_root="F:/HB_2024_SoundFiles/2024"   # to root directory
sourceDir = "F:/HB_2024_SoundFiles/2024/flac"   # to folder containing sound files referenced by path and file in inFile
snipDir = "F:/HB_2024_SoundFiles/2024/snipsOut.2025-1-04"
inFile1 = "Sounds-to-extract.2025-1-04.xlsx"
  inSheet1 = "targets"

setwd(workpath_root)

# Open file with targets for snips
df1_targets <- read_excel(inFile1, sheet = inSheet1)

# Create snipDir if it doesn't exist
if (!dir.exists(snipDir)) {
  dir.create(snipDir, recursive = TRUE)
  print(paste("Directory created:", snipDir))
}

# Copy the file with target data to snipDir
file.copy(inFile1, file.path(snipDir, basename(inFile1)))

# Confirm the file was copied
if (file.exists(file.path(snipDir, basename(inFile1)))) {
  print(paste("File successfully copied to:", snipDir))
} else {
  warning("File copy failed.")
}

# change mmdd to chr with leading zeros
df1_targets$mmdd_chr <- str_pad(
  string = as.character(df1_targets$mmdd),
  width  = 4,
  pad    = "0"
)

nSnipsMax <- nrow(df1_targets)
ffirst=1
flast=nSnipsMax

for (f in ffirst:flast) {   # for each row of input manifest, each with 2 subsequent sound files
  # I was unable to get R to read flac files directly, so used sox to rewrite each flac as a wav file  
  # For this to work, sox must be installed to the system of the computer running this code
  if (exists("audio1")) {
    rm(audio1)
    rm(audio2)
    rm(audio3)
  }

  # Read first flac file  
  # Construct FLAC file path
  flac_file <- file.path(sourceDir, df1_targets$path[f], df1_targets$file[f])
  flac_file <- gsub("//", "/", flac_file)  # Clean up double slashes
  
  # Set output WAV file path
  wav_file <- file.path(getwd(), paste0("output_file_", 1, ".wav"))
  
  # Use SoX to convert FLAC to WAV
  system(paste("sox", shQuote(flac_file), shQuote(wav_file)))
  
  # Read the resulting WAV file
  assign(paste0("audio", 1), readWave(wav_file))
  
  print(paste("Converted and loaded:", wav_file))
  
 ### Open subsequent.file1
  # Construct FLAC file path
  flac_file <- file.path(sourceDir, df1_targets$path[f], df1_targets$subsequent.file1[f])
  flac_file <- gsub("//", "/", flac_file)  # Clean up double slashes
  
  # Set output WAV file path
  wav_file <- file.path(getwd(), paste0("output_file_", 2, ".wav"))
  
  # Use SoX to convert FLAC to WAV
  system(paste("sox", shQuote(flac_file), shQuote(wav_file)))
  
  # Read the resulting WAV file
  assign(paste0("audio", 2), readWave(wav_file))
  
  print(paste("Converted and loaded:", wav_file)) 
  
# Read the two .wav files
wav1 <- readWave("output_file_1.wav")
wav2 <- readWave("output_file_2.wav")

# Combine the two audio files
combined <- bind(wav1, wav2)

# Define the start time in minutes
start_min <- df1_targets$targetStart.min[f]

# Get the Wave's sample rate
srate <- combined@samp.rate

# Convert that one row’s start_min to seconds
start_sec <- start_min * 60

# Convert to sample indices
start_index <- as.integer(start_sec * srate)
end_index   <- as.integer((start_sec + 600) * srate)

# Extract samples for this segment
snip_samples_left  <- combined@left[ start_index : end_index ]

# Create new Wave object
snip <- Wave(left      = snip_samples_left,
             samp.rate = srate,
             bit       = combined@bit,
             pcm       = combined@pcm)

# Construct temporary file name, for example "snip/ABC123_5.wav"
out_wav <- file.path(snipDir, paste0(df1_targets$fileID[f], "_",
                                      df1_targets$plot[f], "_",
                                     df1_targets$year[f],
                                      df1_targets$mmdd_chr[f], "_",
                                     df1_targets$targetStart.min[f], ".wav"))

# Write the snippet to file
writeWave(snip, out_wav)

# 3) Construct the  .flac filename 
out_flac <- file.path(snipDir, paste0(df1_targets$fileID[f], "_",
                                      df1_targets$plot[f], "_",
                                      df1_targets$year[f],
                                      df1_targets$mmdd_chr[f], "_",
                                      df1_targets$targetStart.min[f], ".flac"))

# 4) Use SoX to convert from .wav to .flac
system(paste("sox", shQuote(out_wav), shQuote(out_flac)))

# 5) (Optional) Remove the temporary .wav file
file.remove(out_wav)

} #  next fileID

