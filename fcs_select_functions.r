# Functions for R script 'fcs_analysis_script.r'
# Interactive selection of minutes on false-colour spectrogram images 
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

# User customisation required:
# At Lines 41, 42, 49 & 51 - arguments to 'substr' depend on file name format
# At Line 49 - specifies a search for WAV files - edit if another file extension is used
# At Lines 88, 130 and 161 - need to enter full path of where QUT AnalysisPrograms.exe is located

##############################
# Function 1 - select.fcs
# Select the False-colour spectrogram image file

select.fcs <- function(audio_drive, fcs_drive=NULL){

# select the FCS to view
#fcs_img1 <- file.choose() # gives a character vector of length one giving the file path
# alternative is choose.files() to allow to set default directory
  
  if(is.null(fcs_drive)){ 
    choose.files()
  } else {
    fcs_img1 <- choose.files(default=fcs_drive, multi=FALSE)
  }

  # check if fcs_img1 is empty i.e. file has not been chosen
  if(identical(fcs_img1, character(0))) stop("No file selected - action cancelled")

# extract recording start date-time - arguments are specific to the filename format
	start.date <- substr(basename(fcs_img1), 5, 12)
	start.time <- substr(basename(fcs_img1), 14, 19)
	options(digits.secs=2)
	start.date.time <- paste(start.date, start.time)
# create date-time object with strptime
	rec.start <- strptime(start.date.time, "%Y%m%d %H%M%S")

# Find matching sound file from FCS png file name
file_pattern <- paste(substr(basename(fcs_img1), 1, 19), ".*wav", sep="")

rec_id <- substr(basename(fcs_img1), 1, 24)
  
target.file = list.files(audio_drive, pattern=file_pattern, full.names=TRUE, recursive=TRUE, include.dirs=TRUE)
num.files.found = length(target.file)
target.file = normalizePath(file.path(target.file))

cat("\nFCS image selected:\n", fcs_img1, "\n\nPattern to match:", "'", file_pattern, "'", sep="")
if(num.files.found>0 && num.files.found<11){cat("\n\nMatching files found:", target.file, sep="\n")}

if(num.files.found==0){cat("\n"); print("Warning: No wav files found matching FCS name.") }
if(num.files.found>1) {cat("\n"); print("Warning: More than 1 matching file found.") }

# list items for output
output_list <- list(Image_file=fcs_img1, Recording_Id=rec_id, Matched_file=target.file, Rec_start=rec.start)

return(output_list)

}

#########################################
# Function 2 - select.min
# Select single minute interactively on FCS, cut audio and open

select.min <- function(fcs_img1, rec.id, target.file){

# load selected FCS image for imager package
im1 <- load.image(fcs_img1)

# use grabPoint function to select a point on the image
sel_pt <- grabPoint(im1, output="coord")

minute_selected = sel_pt["x"]-1
start.secs = (sel_pt["x"]-1)*60
end.secs = start.secs+60

# cut minute using start and end - will output as a wav
cut.command <- sprintf('AudioCutter "%s"  "%s" -p FALSE -d 60 -s %i -e %i', target.file, output_folder, start.secs, end.secs)
system2('C:\\AP\\AnalysisPrograms.exe', cut.command)

# reconstruct output file name (Audiocutter determines this)
target.min.file <- paste(output_folder, "\\", rec.id, "_", start.secs, "-", end.secs, ".wav", sep="")

# calc audio and clock times
audio.time <- paste(trunc(minute_selected/60), "hrs_", round((minute_selected/60)%%1*60), "mins", sep="")
clock.time <- night_selected[[4]] + start.secs

# list items for output
output_list <- data.frame(Recording=basename(target.file), Minute=minute_selected, Start.seconds=start.secs, End.seconds=end.secs, Audio.time=audio.time, Selection.Start.Clock.time=clock.time)
cat("\nSelection:\n")
print(output_list, row.names=FALSE)

# open the minute wav
system2("open", target.min.file)

return(rbind(mins.selected, minute_selected, deparse.level = 0))

}

###########################
# Function 3 - select.region
# Select rectangle region on FCS, cut audio and open

select.region <- function(fcs_img1, rec.id, target.file){

# load selected FCS image for imager package
im1 <- load.image(fcs_img1)

# use grabPoint function to select a point on the image
sel_reg <- grabRect(im1, output="coord")

# *** check this
first.min.selected = sel_reg["x0"]-1
last.min.selected = sel_reg["x1"]
start.secs = first.min.selected*60
end.secs = last.min.selected*60
duration.secs = end.secs-start.secs

# cut minute using duration, start and end - will output as a wav
cut.command <- sprintf('AudioCutter "%s"  "%s" -p FALSE -d %i -s %i -e %i', target.file, output_folder, duration.secs, start.secs, end.secs)
system2('C:\\AP\\AnalysisPrograms.exe', cut.command)

# reconstruct output file name (Audiocutter determines this)
target.min.file <- paste(output_folder, "\\", rec.id, "_", start.secs, "-", end.secs, ".wav", sep="")

region.start.hr = paste(trunc(first.min.selected/60), "hrs_", round((first.min.selected/60)%%1*60), "mins", sep="")
region.duration = duration.secs/60
region.start.time = night_selected[[4]]+start.secs
region.output <- data.frame(Recording=basename(target.file), Start_minute = first.min.selected,
                End_minute = last.min.selected-1, Start_audio_time = region.start.hr, 
                Start_clock_time = region.start.time, Duration_mins = region.duration)
							
cat("\nSelection:\n")
print(region.output, row.names=FALSE)

# open the minute wav
system2("open", target.min.file)

}

#########################################
# Function 4 - cut.audio
# Select single minute on FCS, cut audio and open

cut.audio <- function(rec.id, target.file, cut.start, cut.length){

start.secs = cut.start*60
end.secs = start.secs+(cut.length*60)

# cut minute using start and end - will output as a wav
cut.command <- sprintf('AudioCutter "%s"  "%s" -p FALSE -d %i -s %i -e %i', target.file, output_folder, cut.length*60, start.secs, end.secs)
system2('C:\\AP\\AnalysisPrograms.exe', cut.command)

# reconstruct output file name (Audiocutter determines this)
target.min.file <- paste(output_folder, "\\", rec.id, "_", start.secs, "-", end.secs, ".wav", sep="")

start.audio.time <- paste(trunc(cut.start/60), "hrs_", (cut.start/60-(trunc(cut.start/60)))*60, "mins", sep="")
end.audio.time <- paste(trunc(end.secs/3600), "hrs_", (end.secs/3600-(trunc(end.secs/3600)))*60, "mins", sep="")
cut.start.time = night_selected[[4]]+start.secs

output_list <- data.frame(Recording=rec.id, Start.Audio.time=start.audio.time, Start.clock.time=cut.start.time)
cat("\nSelection:\n")
print(output_list, row.names=FALSE)

# open the minute wav
system2("open", target.min.file)

}

#######################
# Calculate duration of calling bout in minutes
# input is fcs_img1 - ie file chosen with select.fcs function

# fcs_img1 is a character vector of FCS file name: e.g. "E:/.../FR3_20120406_180000+1000_ACI-ENT-EVN.png"

count.mins <- function(fcs_img1){

# load selected FCS image for imager package
im1 <- load.image(fcs_img1)

# use grabPoint function to select start point on the image
# outputs the x and y coordinates of the pixel selected

print("Select the first minute", quote=FALSE)
sel_pt <- grabPoint(im1, output="coord")
first.min = sel_pt["x"]-1

print("Select the last minute", quote=FALSE)
sel_pt <- grabPoint(im1, output="coord")
last.min = sel_pt["x"]-1

duration.mins = (last.min-first.min)+1

# Calc start and end time - need to test
calling.start = night_selected[[4]] + first.min*60
calling.end = night_selected[[4]] + last.min*60

cat("\nRecording:", night_selected[[2]])
cat("\nSelection start minute:", first.min, "\nSelection end minute:", last.min)

cat("\n\nSelection start time:")
print(format(unname(calling.start), "%Y-%m-%d %H:%M:%S"))
cat("Selection end time:")
print(format(unname(calling.end), "%Y-%m-%d %H:%M:%S"))
cat("Duration (minutes):", duration.mins)

}

#########################


