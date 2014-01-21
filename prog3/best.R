best <- function(state, outcome) {
  ## Return hospital name in that state with lowest 30-day death rate

  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!state %in% data[["State"]]) stop("invalid state")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  if (outcome == "heart attack") {
    ## Subset 'state' records
    cdata <- subset(data, State == state, select = c(2,7,11))

    ## sort by 30-day mortality for heart attack
    cdata <- cdata[order(as.numeric(cdata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]), cdata[["Hospital.Name"]]),]
  } else if (outcome == "heart failure") {
    cdata <- subset(data, State == state, select = c(2,7,17))
  
    ## sort by 30-day mortality for heart failure
    cdata <- cdata[order(as.numeric(cdata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]), cdata[["Hospital.Name"]]),]
  } else {
    cdata <- subset(data, State == state, select = c(2,7,23))
  
    ## sort by 30-day mortality for pneumonia
    cdata <- cdata[order(as.numeric(cdata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]), cdata[["Hospital.Name"]]),]
  }
  
  return(cdata[1,1])
}
