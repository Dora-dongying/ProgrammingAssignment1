pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating the location of 
  ## the CSV files.
  ## 'pollutant' is a character vector of length 1 indicating the type of 
  ## pollutant, either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID.
  ## Return the mean value of the pollutant level in interested monitor range.
    #setwd(directory)
    if (pollutant == 'sulfate'){
        pollutantlabel <- 2
    }
    else{
        pollutantlabel <- 3
    }    
    sum <- 0 ## set the initial value of sum of means from different monitors
    useful_length <- 0
    for (i in id){
        ## The following is for generating the file name with ID#.
        if (i < 10 && i >= 1){
            file_name <- paste("00", toString(i), ".csv", sep = '')
        }
        else if (i < 100 && i >= 10){
            file_name <- paste("0", toString(i), ".csv", sep = '')
        }
        else if (i <= 332 && i >= 100){
            file_name <- paste(toString(i), ".csv", sep = '')
        }
        else{
          print("Wrong ID range!")
          break
        }
        ## The following is for reading the files.
        data <- read.csv(file_name)
        ##
        data_useful <- data[[pollutantlabel]][complete.cases(data[[pollutantlabel]])]
        sum <- sum + mean(data_useful, na.rm = TRUE) * length(data_useful)
        useful_length <- useful_length + length(data_useful)
    }
    res <- sum/useful_length
    res
}