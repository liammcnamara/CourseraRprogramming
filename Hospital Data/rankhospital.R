rankhospital <- function(state, outcome, num = "best") {
  h_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  i_cond <- c(2, 7, 11, 17, 23)
  
  if(state %in% h_outcome$State) {
    if(outcome %in% conditions) {
      h_outcome <- h_outcome[h_outcome$State == state, ]
      h_outcome <- h_outcome[c(2, 7, 11, 17, 23)]
      h_outcome[, c(3, 4, 5)] <- sapply(h_outcome[, c(3, 4, 5)], as.numeric)
      
      if(outcome == "heart attack") {
        h_outcome <- h_outcome[order(h_outcome[, 3], h_outcome[, 1]), ]
        h_outcome <- h_outcome[!is.na(h_outcome[, 3]), ]
      } else if(outcome == "heart failure") {
        h_outcome <- h_outcome[order(h_outcome[, 4], h_outcome[, 1]), ]
        h_outcome <- h_outcome[!is.na(h_outcome[, 4]), ]
      } else if(outcome == "pneumonia") {
        h_outcome <- h_outcome[order(h_outcome[, 5], h_outcome[, 1]), ]
        h_outcome <- h_outcome[!is.na(h_outcome[, 5]), ]
      }
      
    } else {
      stop("invalid outcome")
    } 
  } else {
    stop("invalid state")
  }
  
  if(num == "best") {
    rankhospital <- as.character(h_outcome[1, 1])
    print(rankhospital)
  } else if(num == "worst") {
    rankhospital <- as.character(h_outcome[nrow(h_outcome), 1])
    print(rankhospital)
  } else if(is.numeric(num)) {
    rankhospital <- as.character(h_outcome[num, 1])
    print(rankhospital)
  }
}