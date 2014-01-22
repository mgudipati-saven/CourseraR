rankhospital <- function(state, outcome, num = "best") {
  ## Return hospital name in that state with the given rank 30-day death rate
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% data[["State"]]) stop("invalid state")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  if (outcome == "heart attack") {
    ## Subset 'state' records
    cdata <- subset(data, State == state, select = c(2,7,11))
    
    ## sort by 30-day mortality for heart attack
    cdata <- cdata[order(as.numeric(cdata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]), cdata[["Hospital.Name"]], na.last = NA),]
  } else if (outcome == "heart failure") {
    cdata <- subset(data, State == state, select = c(2,7,17))
    
    ## sort by 30-day mortality for heart failure
    cdata <- cdata[order(as.numeric(cdata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]), cdata[["Hospital.Name"]], na.last = NA),]
  } else {
    cdata <- subset(data, State == state, select = c(2,7,23))
    
    ## sort by 30-day mortality for pneumonia
    cdata <- cdata[order(as.numeric(cdata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]), cdata[["Hospital.Name"]], na.last = NA),]
  }
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(cdata)
  }
  
  return(cdata[num, 1])
}