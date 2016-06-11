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
    #ordenen zie best.r  --> voor beste=head(n=), voor laatste=tail functie gebruiken
    #zie dat je ook nog 2 checks doet die gevraagd zijn: zie uitroeptekens
  }
}