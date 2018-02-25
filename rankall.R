rankall <- function (outcome, num = "best") {
  # Reading the table and selecting just the columns we need
  outcomeTable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeTable <- outcomeTable[, c(2, 7, 11, 17, 23)]
  
  # Registering key words for invalid outcome detection
  cause <- c("heart attack", "heart failure", "pneumonia")
  
  # Check for invalid outcome
  if (is.element(outcome, cause) == FALSE) {
    stop("invalid outcome")
  }
  
  # Passing the value of the outcome columns as numeric values
  outcomeTable[, 3] <- as.numeric(outcomeTable[, 3])
  outcomeTable[, 4] <- as.numeric(outcomeTable[, 4])
  outcomeTable[, 5] <- as.numeric(outcomeTable[, 5])
  
  # Renaming the columns for easier reading
  names(outcomeTable) <- c("hospital name", "state", "heart attack", "heart failure", "pneumonia")
  
  # Reducing the table according to the outcome
  
  if (outcome == "heart attack") {
    outcomeTable <- outcomeTable[, c(1, 2, 3)]
  } else if (outcome == "heart failure") {
    outcomeTable <- outcomeTable[, c(1, 2, 4)]
  } else if (outcome =="pneumonia") {
    outcomeTable <- outcomeTable[, c(1, 2, 5)]
  }
  
  # Creating a variable containing all states and sorting them
  allState <- unique(outcomeTable[, 2])
  allState <- sort(allState)
  
  # Treating best scenario
  if (num == "best") {
    num <- 1
  }
  
  #Creating the variables that will receive the State and the result for this State and the dataframe
  stateName <- c()
  hospital <- c()
  rankState <- data.frame(hospital, stateName, stringsAsFactors = FALSE)
  
  # Treating "worst" scenario
  if (num == "worst") {
    for (i in allState) {
      # Filtering per state and ordering
      outcomeTableState <- subset(outcomeTable, outcomeTable$state == i)
      outcomeTableState <- outcomeTableState[order(outcomeTableState[, 3], outcomeTableState[, 1], na.last = NA),]
      #Incrementing the variables that will receive the State and the result for this State
      stateName <- c(stateName, i)
      hospital <- c(hospital, outcomeTableState[nrow(outcomeTableState), 1])
      # Incrementing the dataframe
      rankState <- data.frame(hospital, stateName, stringsAsFactors = FALSE)
    }
    
    # Treating other scenarios
  } else {
    for (i in allState) {
      # Filtering per state and ordering
      outcomeTableState <- subset(outcomeTable, outcomeTable$state == i)
      outcomeTableState <- outcomeTableState[order(outcomeTableState[, 3], outcomeTableState[, 1], na.last= NA),]
      #Incrementing the variables that will receive the State and the result for this State
      stateName <- c(stateName, i)
      hospital <- c(hospital, outcomeTableState[num, 1])
      # Incrementing the dataframe
      rankState <- data.frame(hospital, stateName, stringsAsFactors = FALSE)
    }
  }
  # Calling the results
  rankState
}
