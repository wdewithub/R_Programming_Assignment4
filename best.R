best <- function(state, outcome) {
 ##Read outcome data
  setwd("C:/Users/ntpuser3/datascience/R programming/R_Programming_Assignment4")
  input <- read.csv(file="outcome-of-care-measures.csv")
  if (outcome == 'heart attack') {
    outcome_col<-c(2,11)
  }
  else if (outcome == 'heart failure') {
    outcome_col <- c(2,17)
  }
  else if (outcome == 'pneumonia') {
    outcome_col <- c(2,23)
  }
 ##Check that state and outcome are valid
  if (!any(unique(input$State)==state)) {
    stop("invalid state")
  } 
  else if (!any(c('heart attack', 'heart failure', 'pneumonia')==outcome)) {
    stop("invalid outcome")
  } 
 ##Return hospital name in that state with the lowest 30-day death rate
  else {
    state_outcome <- input[input$State==state,outcome_col] 
    if (length(state_outcome[,1]) > 0) #to avoid he does any calc when invalid par were given
    {
      no_null <- state_outcome[state_outcome[,2]!="Not Available",]
      no_null_ordered <- no_null[order(as.numeric(as.character(no_null[,2])),no_null[,1]),]
      best <-  no_null_ordered[1,1]
      print(best)
    }
  }
}