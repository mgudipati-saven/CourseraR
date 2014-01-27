count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) stop("cause is NULL")
  
  ## Check that specific "cause" is allowed; else throw error
  causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  if (!cause %in% causes) stop("invalid cause")
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  r <- grep(paste("Cause: [", toupper(substring(cause, 1, 1)),
                  tolower(substring(cause, 1, 1)), "]",
                  substring(cause, 2), sep = ""),
            homicides)
  
  ## Return integer containing count of homicides for that cause
  return(length(r))
}