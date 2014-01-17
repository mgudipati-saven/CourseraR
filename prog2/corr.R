corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  cors <- numeric()
  comp <- complete(directory)
  met <- comp[, "nobs"] >= threshold
  ids <- comp[met, ][, "id"]
  for (i in ids) {
    data <- getmonitor(i, directory)
    good = complete.cases(data)
    cors <- c(cors, cor(data[good,][["sulfate"]], data[good,][["nitrate"]]))
  }
  return(cors)
}