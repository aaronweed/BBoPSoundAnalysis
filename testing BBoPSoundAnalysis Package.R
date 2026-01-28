# code for testing BBoPSoundAnalysis Package

devtools::install_github("aaronweed/BBoPSoundAnalysis")

library(BBoPSoundAnalysis)
library(magrittr)

# set argument inputs for multiple functions

input_dir <- "D:/"

output_dir<- "C:/Users/aweed/Downloads"

manifest_xlsx<- "C:/Users/aweed/Downloads/cleaned_manifest.xlxs"

# Create manifest from sound files (only takes .flac now but that could be extended to .wav, etc)

?? CreateManifest # check out documentation

manifest<-CreateManifest(input_dir = input_dir, 
                         output_dir = output_dir, 
                         location = "NPS", 
                         technician = "RAL", 
                         export = "object")

# Make Summary Tables per Group (park/location)

?MakeSummaryTables # check out documentation

manifest<-read.csv("C:/Users/aweed/Downloads/soundManifest_BBoP_2024.csv")

MakeSummaryTables(manifest_file = manifest , 
                  manifest_xlsx = manifest_xlsx, 
                  output_dir = output_dir)

# Conduct sound energy by minute

SoundEnergyByMinute(
  manifest_file= manifest,
    flac_root= input_dir,
    output_dir = output_dir,
    temp_dir= output_dir)
