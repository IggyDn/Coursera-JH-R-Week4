best <- function (stateC, outcome) {
  # Reading the table and selecting just the columns we need
  outcomeTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeTable <- outcomeTable[, c(2, 7, 11, 17, 23)]
  # Registering key words for invalid outcome detection
  cause <- c("heart attack", "heart failure", "pneumonia")
  # Check for invalid state or invalid outcome
  if (is.element(stateC, outcomeTable[, 2]) == FALSE) {
    stop("invalid state")
  }
  if (is.element(outcome, cause) == FALSE) {
    stop("invalid outcome")
  }
  # Passing the value of the outcome columns as numeric values
  outcomeTable[, 3] <- as.numeric(outcomeTable[, 3])
  outcomeTable[, 4] <- as.numeric(outcomeTable[, 4])
  outcomeTable[, 5] <- as.numeric(outcomeTable[, 5])
  # Renaming the columns for easier reading
  names(outcomeTable) <- c("hospital name", "state", "heart attack", "heart failure", "pneumonia")
  # Filtering on the state selected
  outcomeTable <- subset(outcomeTable, outcomeTable$state == stateC)
  # Returning the hospital name of the minimum outcome value
  if (outcome == "heart attack") {
    outcomeTable <- outcomeTable[order(outcomeTable[, 3], outcomeTable[, 1], na.last = NA),]
    outcomeTable[1, 1]
  } else if (outcome == "heart failure") {
    outcomeTable <- outcomeTable[order(outcomeTable[, 4], outcomeTable[, 1], na.last = NA),]
    outcomeTable[1, 1]
  } else if (outcome =="pneumonia") {
    outcomeTable <- outcomeTable[order(outcomeTable[, 5], outcomeTable[, 1], na.last = NA),]
    outcomeTable[1, 1]
  }
}
