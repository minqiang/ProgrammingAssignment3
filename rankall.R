rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data <- read.csv("outcome-of-care-measures.csv", as.is=T)
  
  allstates <- levels(factor(data$State))
  m <- length(allstates)
  allnames <- allstates
  
  for (i in 1:m) {
    state <- allstates[i]
    state.data <- subset(data, State==state)
    names <- state.data$Hospital.Name
    if (length(names)==0)
      stop("invalid state")
    
    if (outcome=="heart attack") {
      y <- state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack    
    } else if (outcome == "heart failure") {
      y <- state.data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    } else if (outcome == "pneumonia") {
      y <- state.data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    } else
      stop("invalid outcome")
    
    y <- suppressWarnings(as.numeric(y))
    
    clean.data <- data.frame( names=names, y=y, stringAsFactors=F)
    clean.data <- clean.data[complete.cases(clean.data),]
    ord <- order(clean.data[,2], clean.data[,1])
    
    clean.data <- clean.data[ord,]
    
    allnames[i] <- if(num=="best")
      clean.data$names[[1]]
    else  if(num=="worst") {
      tail(clean.data$names, 1)
    }
    else if (num >0 & num < length(clean.data$names)) {
      clean.data$names[[num]]
    }
    else 
      NA
    
  }
 
  result <- data.frame(allnames, allstates, row.names=allstates)
  names(result) <- c("hospital", "state")
  
  return(result)
  
}

