
# install.packages("tidyverse")
# install.packages("here")
# install.packages("exifr")

# ...................................
# 01.File_manifest.soundFiles.2024.v10.R
# Matt Ayres, 22 September 2022 (30 Dec 2022, 10 Aug 2023, 12 Nov 2023, 22 Dec 2024)
# read.exif requires perl to be installed outside of windows
#       I used Strawberry perl
# This version modified for Miranda's file naming format in 2024
# Code assumes that year_date_time stamp within file names starts 20 characters from the end
# Code assumes that the first part of file name is the area, group, and plot names separated by "_"
# Code assumes that files are in chronological order

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
workpath_root="K:/"   # to root directory
workpath_sub="Raw_Audio_File/2024"                                # subdirectory containing files to manifest
outputFile.pre="soundManifest_"

# Enter universal field names if known, else ""        
    technician="MPoojaP"
    recorderType="Swift"
    plot.alias=""
    recorder=""
    recorderAlias=""
    extraAttrib1=""
    extraAttrib2=""
    directoryLong=""

# ------- end user adjustable parameters ----------
outputFile=paste0(outputFile.pre,workpath_sub,".csv")  
outputFile =  gsub("/", "_", outputFile)
    
# Read attribute data from sound files
# setwd(workpath_root)
# setwd(workpath_sub)
b=paste(workpath_root,"/",workpath_sub,sep="")

file_path = here(b)
setwd(file_path)
    
filePattern="\\.flac$|\\.wav$"
a=read_exif(list.files(file_path, recursive=TRUE, pattern=filePattern, full.names = TRUE, ignore.case = TRUE))


  file_basename=a$FileName
  FileSize.MB=a$FileSize/1000000
  fileLength.min=a$Duration/60
  
  nFiles=nrow(a)    # number of sound files

  # dimension arrays as needed
    FileType=array(data = "", dim = c(nFiles,1))  # diemnsion string array for file types
    nchTotal=array(dim = c(nFiles))  # diemnsion  array for length of file name
    plot=array(data = "", dim = c(nFiles,1))  # diemnsion string array for plot names
    technician=array(data = technician, dim = c(nFiles,1))
    recorderType=array(data = recorderType, dim = c(nFiles,1))
    area=array(data = "", dim = c(nFiles,1))
    group=array(data = "", dim = c(nFiles,1))
    plot.alias=array(data = plot.alias, dim = c(nFiles,1))
    recorder=array(data = recorder, dim = c(nFiles,1))
    recorderAlias=array(data = recorderAlias, dim = c(nFiles,1))
    extraAttrib1=array(data = "", dim = c(nFiles,1))
    extraAttrib2=array(data = "", dim = c(nFiles,1))
    directoryLong=array(data = "", dim = c(nFiles,1))
    directoryShort=array(data = "", dim = c(nFiles,1))
    subsequent.file1_basename=array(data = "", dim = c(nFiles,1))
    subsequent.file2_basename=array(data = "", dim = c(nFiles,1))
    MD5_checksum=array(data = "", dim = c(nFiles,1))
      
# extract file type
    FileType=tolower(a$FileTypeExtension)

#   Parse date and time codes
  # First character of year_date_time = 20th character from end of base file name

      year_date_time <- str_sub(file_basename, -20, -6)
  timeStart=1
  year=str_sub(year_date_time, timeStart, timeStart+3)
  month=str_sub(year_date_time, timeStart+4, timeStart+5)  
  day=str_sub(year_date_time, timeStart+6, timeStart+7)  
  hour=str_sub(year_date_time, timeStart+9, timeStart+10)  
  minute=str_sub(year_date_time, timeStart+11, timeStart+12) 

  startTime.Excel=str_c(month,"-",day,"-",year," ",hour,":",minute)  
  date.mmdd=paste(month, day, sep = "")
  startTime.hhmm=paste(hour, minute, sep = "")
  startTime.yyyymmddhhmm=paste(year,month,day,hour,minute,sep="")

  ############### Parse site, group, and plot names from file name; assumes separated by "_"
    names <- str_sub(file_basename, 1, -21)  
    # Convert to tibble for easy manipulation
    data <- tibble(names = names)
    
    # Separate the 'names' into three new columns
    data <- data %>%
      separate(names, into = c("area", "group", "plot"), sep = "_")

  # extract md5_checksums for files
  for (f in 1:nFiles) {
    lengthFileName=nchar(a$FileName[f])
    lengthRootDir=nchar(workpath_root)
    StartStr=lengthRootDir+2
    EndStr=nchar(a$SourceFile[f])-lengthFileName
    directoryShort[f]=substr(a$SourceFile[f], start = StartStr, stop = EndStr)
    
    MD5_checksum[f] = digest(file(a$SourceFile[f], "rb"), algo = "md5")
    # every thousand files
    if ( f  %% 1000 == 0) {
      print(f)
    }
  }


# check to see if there  are subsequent sound files 
  # check for subsequent file 1
  fLast=nFiles-1
  for (f in 1:fLast) {
    if (date.mmdd[f+1]==date.mmdd[f] & !is.na(fileLength.min[f]) & !is.na(fileLength.min[f+1])) {
      # calculate minutes till next record
      tDiff=as.double(hour[f+1])*60+as.double(minute[f+1])-as.double(hour[f])*60+as.double(minute[f])
        if(tDiff - fileLength.min[f] <2) {
          subsequent.file1_basename[f]=file_basename[f+1]
        }
    }
  }
  
    # check for subsequent file 2
  fLast=nFiles-2
  for (f in 1:fLast) {
    if (date.mmdd[f+2]==date.mmdd[f] & !is.na(fileLength.min[f]) & !is.na(fileLength.min[f+2])) {
      # calculate minutes from base record
      tDiff=as.double(hour[f+2])*60+as.double(minute[f+2])-as.double(hour[f])*60+as.double(minute[f])
      if(tDiff - fileLength.min[f] - fileLength.min[f+2] <2) {
        subsequent.file2_basename[f]=file_basename[f+2]
      }
    }
  }

column_names = c(
  "path",
  "file",
  "subsequent.file1",
  "subsequent.file2",            
  "FileSize.MB",
  "FileType",
  "recorderType",
  "area",
  "group",
  "plot",
  "plot.alias",
  "recorder",
  "recorderAlias",
  "extraAttrib1",
  "extraAttrib2",
  "year",
  "technician",
  "date.mmdd",
  "startTime.hhmm",
  "startTime.Excel",
  "startTime.yyyymmddhhmm",
  "fileLength.min",
  "MD5_checksum"
)

b1 = data.frame(setNames(list(
  directoryShort,
  file_basename,
  subsequent.file1_basename,
  subsequent.file2_basename,
  FileSize.MB,
  FileType,
  recorderType,
  data$area,
  data$group,
  data$plot,
  plot.alias,
  recorder,
  recorderAlias,
  extraAttrib1,
  extraAttrib2,
  year,
  technician,
  date.mmdd,
  startTime.hhmm,
  startTime.Excel,
  startTime.yyyymmddhhmm,
  fileLength.min,
  MD5_checksum), column_names))
    
# filePath	fileName	fSize.Mb	dateTime, dateExcel, ...
b=paste(workpath_root,"/",workpath_sub,sep="")
  setwd(b)
  write.csv(b1, outputFile, row.names = FALSE)
  
######################  

