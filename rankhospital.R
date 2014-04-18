rankhospital <- function(state, outcome, num = "best") {
  options(stringsAsFactors = FALSE)
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- read.csv("outcome-of-care-measures.csv", 
                   as.is=T)
  
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
  
  if(num=="best")
    return(clean.data$names[[1]])
  else  if(num=="worst") {
    return(tail(clean.data$names, 1))
  }
  else if (num >0 & num < length(clean.data$names)) {
    #print(clean.data)
    return(clean.data$names[[num]])
  }
  else 
    return(NA)
  
}