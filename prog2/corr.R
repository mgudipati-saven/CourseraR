corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Create an empty numeric vector
  cors <- numeric()
  
  ## Obtain a data.frame consisting of completed observations for all ids
  comp <- complete(directory)

  ## Subset the data.frame where nobs >= threshold
  met <- comp[, "nobs"] >= threshold
  ids <- comp[met, ][, "id"]

  ## Loop through the ids vector
  for (i in ids) {
    ## Read the data.frame for each id
    data <- getmonitor(i, directory)
    
    ## Compute the logical vector of complete cases 
    good = complete.cases(data)
    
    ## Compute the correlation of sulfate and nitrate observations, append to vector
    cors <- c(cors, cor(data[good,][["sulfate"]], data[good,][["nitrate"]]))
  }
  return(cors)
}