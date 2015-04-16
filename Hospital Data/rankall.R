rankall <- function(outcome, num = "best") {
  h_outcome <- read.csv("outcome-of-care-measures.csv")
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  h_outcome <- h_outcome[c(2, 7, 11, 17, 23)]
  
  states <- unique(h_outcome$State)
  states <- sort(states)

    if(outcome %in% conditions) {
      ranks <- data.frame(hospital=NA, state=NA)
      int <- 1
      for (state in states){
        ranks[int, ] <- c(rankhospital(state, outcome, num), state)
        int <- int + 1
      }
      
      
    } else {
      stop("invalid outcome")
    }   
  
  ranks
}