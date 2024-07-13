# -------------------------------------------------------------------------------------
# Author: Raymart Jay E. Canoy
# Affiliation: Center for Chemical Biology and Biotechnology, University of San Agustin
# Address: Iloilo City, Iloilo 5000, Philippines
# Email: recanoy@alum.up.edu.ph
# GitHub: rcanoy
# Date modified: 18 April 2024
# -------------------------------------------------------------------------------------


average_spectra <- function(WD, filePath, sample_code){
  # Note:
  # This function computes the average absorbance of a UV-VIS spectra and
  # place the result in a dataframe
  #
  # Argument:
  # WD: The directory containing the codes
  # code: The sample code used to assign column names
  # 
  # Output:
  # df0: average absorbance
  #      row: wavelength (nm)
  #      col: sample code
  
  # (00) Clearing the workspace and setting the working dir to folder containing the codes
  rm(list=ls())
  setwd(file.path(WD, 'functions'))
  
  # (01) Setting the working dir to folder containing the spectra
  #cat("\nInput the path of the folder containing the files\n")
  #filePath <- readline(prompt = "Enter the file path: ")
  setwd(filePath)
  
  # (02) Extracting the number of files in the folder containing the spectra
  cat("\nInput the information about your data\n")
  num_files <- length(list.files(filePath))
  num_trials <- readline(prompt = "Number of Trials: ")
  num_samples <- num_files/as.numeric(num_trials)
  
  # (03) Extracting information about the files
  files <- list.files(filePath)
  sample <- read.csv(files[1], header = FALSE, sep = ",")
  num_wavelength = length(sample$V1)
  
  # (04) Constructing an empty dataframe
  df0 <- data.frame(matrix(nrow = num_wavelength-2, ncol = num_samples))
  row.names(df0) <- sample$V1[3:num_wavelength]
  colnames(df0) <- sample_code
  
  # (04) Processing the files
  # Column names for trial
  trial_colNames <- c()
  for (num_trial in 1:num_trials) {
    trial_colNames[num_trial] <- sprintf("T%01d", num_trial)
  }
  
  # Looping through each sample
  for (num_sample in 0:(num_samples-1)) {
    
    # Creating an empty dataframe
    # row: wavelength (nm)
    # col: sample (first col: BLANK)
    df1 <- data.frame(matrix(nrow = num_wavelength-2, ncol = as.numeric(num_trials)))
    row.names(df1) <- sample$V1[3:num_wavelength]
    colnames(df1) <- trial_colNames
    
    # Looping through each trial
    # cat("\nFilename format: T_numSample(02d)_numTrial(01d).csv\n")
    for (num_trial in 1:num_trials){
      fileName <- sprintf("T_%02d_%01d.csv", num_sample, num_trial)
      print(fileName)
      file <- read.csv(fileName, header = FALSE, sep = ",")
      
      df1[[trial_colNames[num_trial]]] <- as.numeric(file$V2[3:num_wavelength])
      # Converting the negative values to zero
      df1[df1 < 0] <- 0
    }
    
    if (num_sample == 0){
      df0[[sample_code[num_sample + 1]]] <- rowMeans(df1)
    } else {
      df0[[sample_code[num_sample + 1]]] <- rowMeans(df1) - df0[[sample_code[1]]]
      df0[df0 < 0] <- 0
    }
  }
  
  # Saving the dataframe to .csv file
  cat("\nEnter the filename of your output *.csv file\n")
  csv_fileName <- readline(prompt = 'Filename of the *.csv file: ')
  csv_fileName_ext <- sprintf('%s.csv', csv_fileName)
  write.csv(df0, file.path(WD, 'output', csv_fileName_ext))
  
  # Returning the dataframe containing the average absorbance
  return(df0)
  
  # Setting back the working directory to the folder containing the codes
  setwd(WD)
}
