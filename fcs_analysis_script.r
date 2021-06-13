# Run functions to open False-colour Spectrogram images and interactively select minutes and open cut audio
# R code written by Sheryn Brodie, James Cook University, 23/09/2020

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

# Requirements:

# requires 'imager' package installed
# requires R file 'fcs_select_functions.r' which has functions: select.fcs, select.min, select.region and cut.audio
#     also has functions count.mins and calc.diff.time
# WAV/MP3 sound files and FCS image files need to have matching file names
# this code written for file names in format: "PRE_YYYYMMDD_hhmmss+1100"

#-------------------
# Prelims and Setup
library(imager) # imager package for grab functions

## USER customisation required:

# i) enter path where functions file is located and load functions file
source("D:/RProjects/FCS_viewer/fcs_select_functions.r")

# ii) set path where sound files are located
my_audio_drive = "D:/RawAudio"

# iii) set a file path where cut audio minutes will be temp saved - NOT in my_audio_drive above
output_folder = "D:/Temp/audio_cuts"

# iv) set default drive where to look for false-colour spectrogram images 
fcs_drive = paste(choose.dir(), "\\*.*", sep="")
# or specify (*.* for all files in directory)
fcs_drive = "D:/FCS_ACI-ENT-EVN_HerveyRange/STO_2014*"

#--------------------
# Interactive Selection Functions

# Select FCS for analysis - outputs a list with file name values
night_selected <- select.fcs(my_audio_drive, fcs_drive); mins.selected<-integer()

# SELECT A MINUTE on FCS to cut and play audio
mins.selected <- select.min(night_selected[[1]], night_selected[[2]], night_selected[[3]])

# select a range of minutes on FCS to cut and play audio
select.region(night_selected[[1]], night_selected[[2]], night_selected[[3]])

# Specify Selection directly
# cut and open audio segment - specify start minute and length in minutes
length.mins=1
start.min=60
cut.audio(night_selected[[2]], night_selected[[3]], start.min, length.mins)

# calculate calling duration from start and end selection
count.mins(night_selected[[1]])

######################


