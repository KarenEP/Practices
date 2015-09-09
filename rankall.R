rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")          ##All are character considered
  data[ ,11]<-as.numeric(data[ ,11])          ##Heart attack
  data[ ,17]<-as.numeric(data[ ,17])          ##Heart failure
  data[ ,23]<-as.numeric(data[ ,23])          ##Pneumonia
  
  ## Check that state and outcome are valid
  outcomes<-c("heart attack","heart failure","pneumonia")
  for(i in 1:3){
    condition<-outcome==outcomes[i]
    if(condition==TRUE){break}
  }
    if(condition==FALSE){stop("Invalid outcome")}
  
  
  
  

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

}