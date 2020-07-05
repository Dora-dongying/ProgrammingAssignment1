corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating the location of 
  ## the CSV files.
  ## 'threshold' is a numeric vector of length 1 indicating the number of completely
  ## observations required to compute the correlation between sulfate and nitrate;
  ## default is zero.
  ## Return a numeric vector of correlations.
    setwd(directory)
    res <- numeric()
    j <- 1 ## Create a vector for res, j is the res index.
    for (i in 1:332){
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
      ## The following is for reading the files.
      data <- read.csv(file_name)
      ## The following is for counting the complete observations.
      complete_ob <- complete.cases(data$sulfate)&complete.cases(data$nitrate)
      complete_N <- length(data$sulfate[complete_ob])
      ## Append the cor info if above the threshold.
      if (complete_N >= threshold && complete_N > 0){
        res[j] <- cor(data$sulfate[complete_ob], data$nitrate[complete_ob])
        j <- j +1
      }
    }
    setwd('..')
    res
}