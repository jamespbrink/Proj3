rankall <- function(outcome, rank="best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  
  hospName <- NULL
  hospState <- NULL
  newRank <- NULL
  
  if (rank == "best") {
    newRank <- 1
  }
  
  states <- sort(unique(data[,7]))
  for ( i in 1:length(states)){
    hospInState <- data[data$State == states[i],]
    hospInState[, column] <- suppressWarnings(as.numeric(hospInState[, column]))
    sortData <- hospInState[order(hospInState[,column], hospInState$Hospital.Name, na.last = NA),]
    if (rank == "worst"){
      newRank <- length(sortData$Hospital.Name)
    }
    if (is.null(newRank) == TRUE){
      newRank <- rank
    }
    hospName <- c(hospName, sortData[newRank,2])
    hospState <- c(hospState, states[i])
  }
  data.frame(hospital = hospName, state = hospState, hospState, row.names = 3)
}