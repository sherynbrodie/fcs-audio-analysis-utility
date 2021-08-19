# Functions required for R audio analysis utility 'fcs_analysis_script.r'
# Interactive selection of minutes on false-colour spectrogram images 

# R code written and provided by Sheryn Brodie, James Cook University, 19/08/21
#  Copyright 2021 Sheryn Brodie
#  Licensed under the Apache License, Version 2.0 (the "License");
#   You may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#       http://www.apache.org/licenses/LICENSE-2.0
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS, 
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# Uses QUT Ecoacoustics Audio Analysis Programs (https://github.com/QutEcoacoustics/audio-analysis)

# Assumptions:
# 'imager' package for R is installed (for grabPoint and grabRect functions)
# QUT Ecoacoustics Audio 'AnalysisPrograms.exe' file is downloaded and saved in path C:/AP (for 'audio2csv' and 'Audiocutter' functions)
#	(otherwise user customisation is required at Line 119)
# Audio files to be analysed are in .WAV, .MP3, .FLAC, .WMA or .OGG format
# Sound files have unique names with a prefix, and valid start date and time (+UTC offset)
#    - of the format "PRE_yyyymmmdd_hhmmss+1000.wav"
# Sound files and FCS image files have matching file names,
#       e.g. 'PRE_20181201_1900+1000.wav' file has a matching FCS named 'PRE_20181201_1900+1000.png'
#		(this should be the case if FCS were generated using QUT AP.exe audio2csv function)

# sample rate can be changed at Line 115 (default set to 22050 Hz)

#########################
# Function 1 - select.fcs
# Select the False-colour spectrogram image file

select.fcs <- function(fcs_drive=NULL, audio_drive){

	# check audio_drive exists
	if(dir.exists(audio_drive)==FALSE){
	cat("\n  Warning: The audio drive specified does not exist.\n")
	}
	
	# select the FCS to view
	if(is.null(fcs_drive)){ 
    	choose.files()
  	} else {
    	fcs_img <- choose.files(default=fcs_drive, multi=FALSE)
  	}

  	# check if fcs_img is empty i.e. file has not been chosen
 	 if(identical(fcs_img, character(0))) stop("No file selected - action cancelled")

	match.wav(fcs_img, audio_drive)
}

########################
# Function 2 - go.to.next
# go to next FCS in file list
go.to.next <- function(current.fcs, audio_drive){
  
	current.fcs.dir <- dirname(current.fcs)
	this.fcs <- which(list.files(current.fcs.dir)==basename(current.fcs))
	fcs_img <- list.files(current.fcs.dir, full.names=TRUE)[this.fcs+1]
	match.wav(fcs_img, audio_drive)
  
}

#######################
# Function 3 - find matching wav file
match.wav <- function(fcs_img, audio_drive){

	# extract recording start date-time
	# match string of numbers to fit date-time format YYYYmmdd.HHMMSS
	start.date.time <- gsub(".*((19|20)[0-9]{6}.[0-9]{6}).*$", "\\1", basename(fcs_img))

	# Extract pattern for matching to sound file - removes all characters after the date-time	
	file_pattern <- gsub(paste0("(.*",start.date.time, ").*$"), "\\1", basename(fcs_img))
	# add file extention options to search for
	file_pattern <- paste0(file_pattern, ".*\\.wav|", file_pattern, ".*\\.mp3|", file_pattern, ".*\\.ogg|",
	                       file_pattern, ".*\\.flac|", file_pattern, ".*\\.wma")

	# convert recording date time to a POSIX object
	# extract just digits
	start.date.time <- gsub("\\D", " ", start.date.time)
	options(digits.secs=2)
	# create date-time object with strptime
	rec.start <- strptime(start.date.time, "%Y%m%d %H%M%S")
	if(is.na(rec.start)){cat("\n");print("Could not parse date-time from filename", quote=F)}
	
	matched.wav = list.files(audio_drive, pattern=file_pattern, full.names=TRUE, recursive=TRUE, include.dirs=TRUE)
	matched.wav = normalizePath(file.path(matched.wav))
	num.files.found = length(matched.wav)

	# give some output info
	cat("\nFCS image selected:\n", fcs_img, "\n\nPattern to match:", "'", file_pattern, "'", sep="")
	if(num.files.found>0 && num.files.found<11){cat("\n\nMatching files found:", matched.wav, "", sep="\n")}

	if(num.files.found==0){cat("\n\n  Warning: No files found matching FCS name.\n\n") }
	if(num.files.found>1) {cat("\n\n  Warning: More than 1 matching file found. First will be used.\n\n") }

	# get filename without file extension - for naming cut segments later
	matched.wav.prefix <- gsub("\\..*$", "", basename(matched.wav)) #works for different length file extensions - e.g. .wav vs .flac

	# list items for output
	output_list <- list(FCS_image=fcs_img, Matched_wav=matched.wav, Rec_start=rec.start, wav_basename=matched.wav.prefix)

	return(output_list)
}

############################
# Function 4 - cut.selection
# Invokes C:/AP/AnalysisPrograms.exe -Audiocutter and opens cut wav

# -r|--sample-rate  (in Hz, default=22050; values up to 96000 are valid).

cut.selection <- function(input_file, start.secs, end.secs, wav_basename, length.secs=60, samp.rate=22050){
  
	# cut minute using start and end seconds - will output as a wav
	cut.command <- sprintf('AudioCutter "%s"  "%s" -p FALSE -d %i -s %i -e %i -r %i', input_file, output_folder, length.secs, start.secs, end.secs, samp.rate)
	system2('C:\\AP\\AnalysisPrograms.exe', cut.command)
  
	# reconstruct output file name (Audiocutter determines name)
	audio_cutting <- paste0(output_folder, "\\", wav_basename, "_", start.secs, "-", end.secs, ".wav")
  
	# open the minute wav
	system2("open", audio_cutting)
}


##########################
# Function 5 - select.min
# Select single minute interactively on FCS, cut audio and open

select.min <- function(FCS_image, Matched_wav, Rec_start, wav_basename){

	# load selected FCS image for imager package
	fcs_img <- imager::load.image(FCS_image)

	# use grabPoint function to select a point on the image
	sel_pt <- imager::grabPoint(fcs_img, output="coord")
	# calculate time of selected point
	minute_selected = sel_pt["x"]-1
	start.secs = (sel_pt["x"]-1)*60
	end.secs = start.secs+60

	# call cut selection function using Audiocutter
	cut.selection(Matched_wav[1], start.secs, end.secs, wav_basename[1])

	# calculate times for output info
	# %/% is integer division; %% is modulus (gives remainder)
	audio.time <- paste0(minute_selected%/%60, "hrs_", minute_selected%%60, "mins")
	clock.time <- Rec_start + start.secs

	# give some output info on console
	output_list <- data.frame(Recording=basename(Matched_wav[1]), Minute=minute_selected, Audio.time=audio.time, Selection.Start.Clock.time=clock.time)
	cat("Selection:\n"); print(output_list, row.names=FALSE); cat("\n")

	# return dataframe for log
	return(data.frame(wav.file=basename(Matched_wav[1]), minute.viewed=minute_selected, length.mins=1))
}

###########################
# Function 6 - select.region
# Select rectangle region on FCS, cut audio and open

select.region <- function(FCS_image, Matched_wav, Rec_start, wav_basename){

	# load selected FCS image for imager package
	fcs_img <- imager::load.image(FCS_image)

	# use grabPoint function to select a point on the image
	sel_reg <- imager::grabRect(fcs_img, output="coord")

	# calculate minutes selected for passing to Audiocutter
	first.min.selected = sel_reg["x0"]-1
	last.min.selected = sel_reg["x1"]
	start.secs = first.min.selected*60
	end.secs = last.min.selected*60
	duration.secs = end.secs-start.secs

	# call cut.selection function using Audiocutter
	cut.selection(Matched_wav[1], start.secs, end.secs, wav_basename[1], duration.secs)

	# calculate times for output info
	region.start.hr = paste0(first.min.selected%/%60, "hrs_", first.min.selected%%60, "mins")
	region.duration = duration.secs/60
	region.start.time = Rec_start+start.secs

	# give some output info on console
	region.output <- data.frame(Recording=basename(Matched_wav[1]), Start_minute = first.min.selected, End_minute = last.min.selected-1, Start_audio_time = region.start.hr, Start_clock_time = region.start.time, Duration_mins = region.duration)
	cat("Selection:\n"); print(region.output, row.names=FALSE); cat("\n")

	# return dataframe for log
	return(data.frame(wav.file=basename(Matched_wav[1]), minute.viewed=first.min.selected, length.mins=region.duration))
}

#########################################
# Function 7 - cut.audio
# Select minute/s on FCS by input of values, cut audio and open
# cut.start and cut.length are provided by user in minutes

cut.audio <- function(FCS_image, Matched_wav, Rec_start, wav_basename, cut.start, cut.length){

	start.secs = cut.start*60
	end.secs = start.secs+(cut.length*60)

	# call cut selection function using Audiocutter
	cut.selection(Matched_wav[1], start.secs, end.secs, wav_basename[1], cut.length*60)

	# calculate times for output info
	start.audio.time <- paste0(cut.start%/%60, "hrs_", cut.start%%60, "mins")
	cut.start.time = Rec_start+start.secs

	# give some output info
	output_list <- data.frame(Recording=basename(Matched_wav[1]), Minute=cut.start, Start.Audio.time=start.audio.time, Start.clock.time=cut.start.time)
	cat("Selection:\n"); print(output_list, row.names=FALSE); cat("\n")

	# return dataframe for output
	return(data.frame(wav.file=basename(Matched_wav[1]), minute.viewed=cut.start, length.mins=cut.length))
}

#######################
# Function 8 - Calculate duration of calling bout in minutes
# select two points on false-colour spectrogram and calculate extent in whole minutes

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

	# Calc start and end time
	calling.start = files_selected[[3]] + first.min*60
	calling.end = files_selected[[3]] + last.min*60

	# give some output info
	cat("\nRecording:", basename(files_selected[[2]]))
	cat("\nSelection start minute:", first.min, "\nSelection end minute:", last.min)
	cat("\n\nSelection start time:")
	print(format(unname(calling.start), "%Y-%m-%d %H:%M:%S"))
	cat("Selection end time:")
	print(format(unname(calling.end), "%Y-%m-%d %H:%M:%S"))
	cat("Duration (minutes):", duration.mins)
}

###


