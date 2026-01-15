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

MakeSummaryTables( inFile = "C:/Users/aweed/Downloads/soundManifest_2025.csv" , workpath_root = "C:/Users/aweed/Downloads")


# Conduct sound energy by minute

SoundEnergyByMinute(
    manifest,
    flac_files= flac_files,
    output_dir = "/output",
    start_filter = 500,
    y_limits = c(2.3, 4.0),
    max_minutes = 180,
    parallel = FALSE)
