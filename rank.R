best <- function(state, outcome, rank){
  ## read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## check that state and outcome are valid
  ## throw error and stop and print invalid state if state is invalid
  ## throw error and stop and print invalid outcome if outcome is invalid
  if(outcome == "heart failure") {
    column <- 17
  }
  else if (outcome == "heart attack") {
    column <- 11
  }
  else if (outcome == "pneumonia") {
    column <- 23
  }
  else {
    stop("invalid outcome")
  }
  
  numHospInState <- sum(state == data[,7])
  if (numHospInState == 0) {
    stop("invalid state")
  }
  
  if (rank == "best") {
    rank <- 1
  }
  if (rank == "worst"){
    rank <- numHospInState
  }
  
  ##return hospital name in that state with lowest 30 day death rate
  ##tie goes to the first hospital alphabetically
  
  hospInState <- data[state == data[,7],]
  hospInState[, column] <- as.numeric(hospInState[, column])
  sortData <- hospInState[order(hospInState[,column], hospInState$Hospital.Name),]
  sortData[rank,2]
}