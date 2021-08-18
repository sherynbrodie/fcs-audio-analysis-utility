# R Code written and provided by Sheryn Brodie on 15/08/2021.
# Adapted from QUT Ecoacoustics Research Group - Ecoacoustics Congress 2018 Workshop 7
# https://ap.qut.ecoacoustics.info/tutorials/01-usingap/practical?tabs=windows

# Function to calculate acoustic indices and generate false-colour spectrograms for a folder of audio files.
# Requires QUT Ecoacoustics Audio Analysis Programs saved in path: 'C:\AP'
#  this is available at: (https://github.com/QutEcoacoustics/audio-analysis)

# the 'generate.fcs' function requires the user to provide two arguments:
# 1) audio.directory = the input directory containing the audio files to be analysed
# 2) rawoutput.directory = the output directory where results are to be saved (Not in input audio directory; will be created if doesn't exist)
# set optional argument add.hiRes=TRUE if additional high resolution output is required

#-------------------------

generate.fcs <- function(audio.directory, output.directory, add.hiRes=FALSE) {
  
  # Get a list of audio files in the directory
  audio.files <- list.files(audio.directory, pattern = "*.wav|*.flac|*.mp3|*.wma|*.ogg", recursive=TRUE, full.names = TRUE, ignore.case=TRUE)
  
  if(length(audio.files) == 0){
    cat("\n"); stop("Warning: No audio files found in folder")
  }
  
  # Start of analysis loop - iterate through each file
    for(i in 1:length(audio.files)) {

    file <- audio.files[i]

    # print progress
    cat('\n\nProcessing recording', i, 'of', length(audio.files), "--", file, '\n')
        
    # parse base name of file
    file_name <- basename(file)
    
    # make a folder for results - name contains audio file name
    output_folder <- normalizePath(file.path(output.directory, "Indices", file_name), mustWork = FALSE)
    
    # prepare command
    # can also specify start and end offset with -s and -e options
    command <- sprintf("audio2csv %s C:/AP/ConfigFiles/Towsey.Acoustic.yml %s -l 1 --parallel --when-exit-copy-config", paste0('"', file, '"'), paste0('"', output_folder, '"'))
    
    # execute the command
    system2('C:\\AP\\AnalysisPrograms.exe', command)
    
    # find the ACI-ENT-EVN false-colour spectrogram and copy to main output folder
    fcs.png <- list.files(output_folder, pattern="_ACI-ENT-EVN\\.png", recursive=TRUE, full.names = TRUE)
    file.copy(fcs.png, output.directory)
	
	 	# Generate hiRes spectrograms
		if(add.hiRes==TRUE){
	  
		# this inner statement is working on the current 'output_folder'
		input.folder <- normalizePath(file.path(output_folder, "Towsey.Acoustic"))

		# make an output folder for results - name contains audio file name (will be created of doesn't exist)
		hiRes.dir <- normalizePath(file.path(output.directory, "HiRes"), mustWork = FALSE)
		hiRes.folder <- normalizePath(file.path(hiRes.dir, basename(output_folder)), mustWork = FALSE)

		#construct command - using paste to break over multiple lines
		command <- paste(sprintf("DrawLongDurationSpectrograms -i %s -o %s", input.folder, hiRes.folder),
					   " -fcs C:/AP/ConfigFiles/SpectrogramFalseColourConfig.yml",
					   " -ip C:/AP/ConfigFiles/IndexPropertiesConfig.HiRes.yml", sep="")
		  
		 # execute the command
		 system2('C:\\AP\\AnalysisPrograms.exe', command)
	  
		# find the hiRes spectrogram and copy out
		hiRes.results <- list.files(hiRes.folder, pattern="_ACI-ENT-EVN\\.png", full.names = TRUE)
		file.copy(hiRes.results, hiRes.dir)
			  
	  } 
	
  }
  # end of loop
}

#----------------------
