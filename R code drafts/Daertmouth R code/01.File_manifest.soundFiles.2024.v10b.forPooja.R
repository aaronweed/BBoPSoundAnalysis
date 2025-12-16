
# ...................................
# 01.File_manifest.soundFiles.2024.v10a.R (modified for Pooja NPS)
# Matt Ayres, 22 September 2022 (30 Dec 2022, 10 Aug 2023, 12 Nov 2023, 22 Dec 2024)
# read.exif requires perl to be installed outside of windows
#       I used Strawberry perl
# Code only processes flac files (not wav files)
# Code assumes that year_date_time stamp within file names starts 20 characters from the end
# Code assumes that "group" = first four chars of fileName and "plot" = chars 5:8
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

workpath_out = "//dartfs-hpc/rc/lab/a/AyresM/Root/birdSound_HBEF/2023"    #  directory for output
workpath_in = "G:/HB"  # directory containing files to manifest

# Enter universal field names if known, else ""        
    area_pre = "HB"
    year = 2023
    technician="PoojaP"
    recorderType="SwiftOne"
    plot.alias=""
    recorder=""
    recorderAlias=""
    extraAttrib1=""
    extraAttrib2=""
    directoryLong=""

outputFile.pre="soundManifest_HB_"    
    

# ------- end user adjustable parameters ----------
outputFile=paste0(workpath_out, "/", outputFile.pre,year,".csv")  
    
# Read attribute data from sound files
setwd(workpath_in)
    
filePattern="\\.flac$"
a=read_exif(list.files(workpath_in, recursive=TRUE, pattern=filePattern, full.names = TRUE, ignore.case = TRUE))


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

  ############### Parse site, group, and plot names from file name; assumes chars 1-4 and 5-8
    names <- str_sub(file_basename, 1, -21)  
    # Convert names to tibble for easy manipulation
    data <- tibble(names = str_sub(file_basename, 1, -21)) %>%
      mutate(
        group = str_sub(names, 1, 4),    # Extract first 4 characters
        plot = str_sub(names, 5, 8)     # Extract characters 5 to 8
      )

    # Fill the area column with the value of area_pre
    data <- data %>%
      mutate(area = area_pre)
    
  # extract md5_checksums for files
  for (f in 1:nFiles) {
    lengthFileName=nchar(a$FileName[f])
    lengthRootDir=nchar(workpath_in)
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

  write.csv(b1,outputFile, row.names = FALSE)
  
######################  

