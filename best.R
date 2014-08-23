best <- function(state, outcome){
        ## read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## check that state and outcome are valid
        ## throw error and stop and print "invalid state" if state is invalid
        ## throw error and stop and print "invalid outcome" if outcome is invalid
        sum(state == data[,7])
        
        ##return hospital name in that state with lowest 30 day death rate
        ##tie goes to the first hospital alphabetically
        
}