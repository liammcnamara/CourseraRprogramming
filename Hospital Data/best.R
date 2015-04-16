
best <- function(state, outcome) {
  
  h_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  i_cond <- c(11, 17, 23)

  if(state %in% h_outcome$State) {
    if(outcome %in% conditions) {
      h_outcome <- h_outcome[h_outcome$State == state, ]
      h_outcome <- h_outcome[order(h_outcome[ ,2]), ]
      ## h_outcome[, i_cond] <- sapply(h_outcome[, i_cond], as.numeric)
      if(outcome == "heart attack") {
        best <- h_outcome[which.min(h_outcome[, i_cond[1]]), "Hospital.Name"]
        best <- as.character(best)
        print(best)
      } else if(outcome == "heart failure") {
        best <- h_outcome[which.min(h_outcome[, i_cond[2]]), "Hospital.Name"]
        best <- as.character(best)
        print(best)
      } else if(outcome == "pneumonia"){
        best <- h_outcome[which.min(h_outcome[, i_cond[3]]), "Hospital.Name"]
        best <- as.character(best)
        print(best)
      }
    } else {
      stop("invalid outcome")
    }
    
  } else {
    stop("invalid state")
  }
}