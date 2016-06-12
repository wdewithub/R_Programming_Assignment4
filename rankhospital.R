rankhospital <- function(state, outcome, num) {
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
  ##Return hospital name that has the requested ranking for the specified outcome variable
  else {
    state_outcome <- input[input$State==state,outcome_col] 
    #filter hospitals with no data on outcome
    no_null <- state_outcome[state_outcome[,2]!= 'Not Available',]
    #determine number of hospitals left
    num_hosp <- nrow(no_null)
    #give back hospital on requested rank or na if requested rank is higher than number of hospitals
    no_null_ordered <-no_null[order(as.numeric(as.character(no_null[,2])),no_null[,1]),]    
    if (num =='best') {
        print(head(no_null_ordered[,1],n=1))
    } 
    else if (num =='worst') {
        print(tail(no_null_ordered[,1],n=1))       
    }
    else {
      if (num > num_hosp) {
        stop("NA")
      }
      else {
      print(no_null_ordered[num,1])
      }
    }
  }
}