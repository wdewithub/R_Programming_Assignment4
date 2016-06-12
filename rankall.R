rankall <- function (outcome, num=2) {
  ##Read outcome data
  setwd("C:/Users/ntpuser3/datascience/R programming/R_Programming_Assignment4")
  input <- read.csv(file="outcome-of-care-measures.csv")
  if (outcome == 'heart attack') {
    outcome_col<-c(2,7,11)
  }
  else if (outcome == 'heart failure') {
    outcome_col <- c(2,7,17)
  }
  else if (outcome == 'pneumonia') {
    outcome_col <- c(2,7,23)
  }
  ##Check that a valid outcome was given
  ##If no valid outcome given, report an error message:
  if (!any(c('heart attack', 'heart failure', 'pneumonia')==outcome)) {
    stop("invalid outcome")
  } 
  ##Else: return a dataframe with for each state the hospital that has the requested ranking
  ##for the specified outcome variable
  else {
    #filter all hospitals with no data for the requested outcome
    col_sel <- input[,outcome_col]
    no_null <- col_sel[col_sel[,3] != 'Not Available',]
    #sort by state the remaining hospitals by the value of the requested outcome
    ordered <- no_null[order(no_null[,2],as.numeric(as.character(no_null[,3])),no_null[,1]),]
    #give for each state the hospital at the requested rank
      statelist <- unique(no_null$State)
      for (i in 1:length(statelist))
      {
        state_sel <- levels(statelist)[i]
        state_rank_base <- ordered[ordered$State==state_sel,1:2]
        #determine number of hospitals left
        num_hosp <- nrow(state_rank_base)
        #give back hospital on requested rank or na if requested rank is higher than number of hospitals
        if (num =='best') {
          state_rank <- state_rank_base[1,]
        } 
        else if (num =='worst') {
          state_rank <- state_rank_base[nrow(state_rank_base),]
        }
        else {
          if (num > num_hosp) {
            state_rank <- c(NA, state_sel)
          }
          else {
            state_rank <- state_rank_base[num,]
          }
        }
        #Add result of this state to output dataframe
        if (i==1) {
          result <- state_rank
        }
        else {
          result <- rbind(result,state_rank)
        }
      }
      names(result) <- c("hospital","state" )    
      result
  }
}
