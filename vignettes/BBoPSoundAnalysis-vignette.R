## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.width = 7,
fig.height = 4
)


## ----install, eval=F----------------------------------------------------------
# # install.packages("devtools") # if needed
# devtools::install_github("aaronweed/BBoPSoundAnalysis", build_vignettes = TRUE)

## ----load, eval=F-------------------------------------------------------------
# library(BBoPSoundAnalysis)

## ----CreateManifest, eval=F---------------------------------------------------
# 
# sound_dir <- "path/to/sound/files"
# manifest <- CreateManifest(sound_dir , output_dir = "/output", year = 2023, location = "Park A", technician = "RAL")
# head(manifest)
# 

## ----MakeSummary, eval=F------------------------------------------------------
# 
# summary_tbl <- MakeSummaryTables(manifest_file = manifest, output_dir = "output/cleaned_manifest.xlsx", workpath_root = "output/")
# summary_tbl

## ----soundenergy, eval=F------------------------------------------------------
# energy_by_minute <- SoundEnergyByMinute(manifest)

## ----viz, eval=F--------------------------------------------------------------
# library(ggplot2)
# ggplot(energy_by_minute, aes(x = minute, y = energy)) +
# geom_line() +
# labs(title = "Sound Energy by Minute",
# x = "Minute",
# y = "Energy"
# )

