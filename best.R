best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
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
  }
  
  y <- suppressWarnings(as.numeric(y))
  if(length(y)==0)
    stop("invalid outcome")
  
  clean.data <- data.frame( names=names, y=y)
  clean.data <- clean.data[complete.cases(clean.data),]
  ord <- order(clean.data[,2], clean.data[,1])
  clean.data <- clean.data[ord,]
  clean.data$names[[1]]

}