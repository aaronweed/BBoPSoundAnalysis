# code for testing BBoPSoundAnalysis Package

devtools::install_github("aaronweed/BBoPSoundAnalysis")

library(BBoPSoundAnalysis)
library(magrittr)

# set argument inputs for multiple functions

input_dir <- "D:/"

output_dir<- "C:/Users/aweed/Downloads"

# Create manifest from sound files (only takes .flac now but that could be extended to .wav, etc)

?? CreateManifest # check out documentation

manifest<-CreateManifest(input_dir = input_dir, 
                         output_dir = output_dir, 
                         location = "NPS", 
                         technician = "LauraS", 
                         export = "csv")

# Make Summary Tables per Group (park/location)

??MakeSummaryTables # check out documentation

MakeSummaryTables( inFile = "C:/Users/aweed/Downloads/soundManifest_2025.csv" , workpath_root = output_dir )

# Conduct sound energy by minute

SoundEnergyByMinute(
  manifest_file= manifest,
    flac_root= input_dir,
    output_dir = output_dir,
    temp_dir= output_dir)
