# Run script to open False-colour Spectrogram images and interactively select minutes and open cut audio

# R code written and provided by Sheryn Brodie, 19/08/21
#  Copyright 2020 Sheryn Brodie
#  Licensed under the Apache License, Version 2.0 (the "License");
#   You may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#       http://www.apache.org/licenses/LICENSE-2.0
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS, 
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# This R code is written for use in RStudio
# It calls several user-defined functions in the file 'fcs_select_functions.r'
# The functions allow interactive selection of minutes on False-colour Spectrogram (FCS) images
# Selected segments of audio are opened in the user's default audio file player

# Assumptions:

# Accompanying R file 'fcs_select_functions.r' is saved somewhere
# 'imager' package for R is installed
# QUT Ecoacoustics 'AnalysisPrograms.exe' file is downloaded and saved in path C:/AP (for 'Audiocutter' function)
# Audio files to be analysed are in .WAV, .MP3, .FLAC, .WMA or .OGG format
# Sound files have unique names with a prefix, and valid start date and time (+UTC offset)
#    - of the format "PRE_yyyymmmdd_hhmmss+1000.wav"
# Sound files and FCS image files have matching file names,
#       e.g. 'PRE_20181201_1900+1000.wav' file has a matching FCS named 'PRE_20181201_1900+1000.png'
#		(this should be the case if FCS were generated using QUT AP.exe audio2csv function)

#-------------------
# Prelims and Setup

# load required library - imager package for grab functions
library(imager) 

## USER customisation required for setting the following file paths:

# load functions file
source("C:/R_scripts/fcs_select_functions.r")

# specify file path where audio files are located
my_audio_drive = "D:/Recordings"

# set a file path where cut audio minutes will be temp saved - NOT in my_audio_drive above
output_folder = "D:/Temp/audio_cuts"

# set default path where to look for false-colour spectrogram images 
fcs_path = paste(choose.dir(), "\\*.*", sep="")

# OR narrow search to some files (*.* shows all files in directory)
fcs_path = "D:/Analyses/FCS/Pre_2018*"

#--------------------
# create/reset log - stores history of selections
view.log <- data.frame(wav.file=character(), minute.viewed=integer(), length.mins=integer())

# Select FCS image for analysis - outputs a list with working file name values
files_selected <- select.fcs(fcs_path, my_audio_drive)

# GO TO NEXT FCS in current list
files_selected <- go.to.next(files_selected$FCS_image, my_audio_drive)

#--------------------
# Interactive Selection Functions

# SELECT MINUTE on FCS to cut and play a single minute segment
view.log <- rbind(view.log, do.call(select.min, files_selected),make.row.names=F)

# SELECT A REGION on FCS to cut and play several minutes
view.log <- rbind(view.log, do.call(select.region, files_selected),make.row.names=F)

# Specify minute to cut and open - set start minute and length in minutes
length.mins = 10
start.min = 0
view.log <- rbind(view.log, do.call(cut.audio, c(files_selected, start.min, length.mins)),make.row.names=F)

# calculate duration between two selected time points
count.mins(files_selected[[1]])

#------------------------------
# For info - view log of selections in date-time order
view.log[order(view.log$wav.file, view.log$minute.viewed),]

###


