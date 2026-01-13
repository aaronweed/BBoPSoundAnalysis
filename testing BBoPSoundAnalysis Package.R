# code for testing BBoPSoundAnalysis Package

devtools::install_github("aaronweed/BBoPSoundAnalysis")

library(BBoPSoundAnalysis)



# set argument inputs for multiple functions

flac_files <- "D:/NPS_Audio/2021/ACAD/ACAD3001"

manifest_output<- "C:/Users/aweed/Downloads"

# Create manifest from sound files (only takes .flac now but that could be extended to .wav, etc)

?? CreateManifest # check out documentation

manifest<-CreateManifest(input_dir = flac_files, 
                         output_dir = manifest_output, 
                         location = "NPS", 
                         technician = "LauraS", 
                         export = "csv")

# Make Summary Tables per Group (park/location)

??MakeSummaryTables # check out documentation

MakeSummaryTables( "data/manifest.csv" , workpath_root = getwd())


# Conduct sound energy by minute

SoundEnergyByMinute(
    manifest,
    flac_files= flac_files,
    output_dir = "/output",
    start_filter = 500,
    y_limits = c(2.3, 4.0),
    max_minutes = 180,
    parallel = FALSE)
