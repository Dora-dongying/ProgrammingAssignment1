complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating the location of 
  ## the CSV files.
  ## 'id' is an integer vector indicating the monitor ID.  
  ## Return a data frame of this form:
  ## id   nobs
  ## 1    117
  ## 2    1041
  ## ...
    setwd(directory)
    my_nobs <- integer(length = length(id))
    j <- 1
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
      ## The following is for calculate the none NA sum and none NA # of values.
      my_nobs[j] <- length(data$sulfate[complete.cases(data$sulfate)&complete.cases(data$nitrate)])
      j <- j + 1
    }
    res <- data.frame("id" = id, "nobs" = my_nobs)
    setwd('..')
    res
}
