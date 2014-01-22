rankall <- function(outcome, num = "best") {
  ## Return a data frame with the hospital names and the (abbreviated) state name}
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")

  if (outcome == "heart attack") {
    oid <- 11
    oname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    oid <- 17
    oname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {
    oid <- 23
    oname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }

  state <- levels(factor(data[["State"]]))
  hospital <- character()
  ## For each state, find the hospital of the given rank
  for (i in state) {
    ## Subset 'state' records
    cdata <- subset(data, State == i, select = c(2,7,oid))

    ## sort by 30-day mortality for the specified outcome
    cdata <- cdata[order(as.numeric(cdata[[oname]]), cdata[["Hospital.Name"]], na.last = NA),]

    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- nrow(cdata)
    } else {
      rank <- num
    }

    hospital <- c(hospital, cdata[rank, 1])
  }
  
  return(data.frame(hospital, state))
}