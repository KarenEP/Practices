rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")          ##All are character considered
  data[ ,11]<-as.numeric(data[ ,11])          ##Heart attack
  data[ ,17]<-as.numeric(data[ ,17])          ##Heart failure
  data[ ,23]<-as.numeric(data[ ,23])          ##Pneumonia
  
  ## Check that state and outcome are valid
  outcomes<-c("heart attack","heart failure","pneumonia")
  for(i in 1:3){
    condition<-outcome==outcomes[i]                 ##Logical vector to check outcome given
    if(condition==TRUE){break}                      ##Correct outcome given break the loop
  }
    if(condition==FALSE){stop("Invalid outcome")}   ##Incorrect outcome given stop the function
  
  states<-unique(data[ ,"State"])                   ##Character vector of all states(no repeted)
  nstates<-length(states)                           ##Number of states [54]
  
  out<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  x<-out[outcome]                 
  rank<-data.frame()
  
  ## For each state, find the hospital of the given rank
  for(i in 1:nstates){                              ##Loop for all states
   dataout<-data[data[ ,"State"]==states[i], ]    ##Only data of the state loop given
   list<-dataout[ ,c(2,7,x)]                      ##Data frame of hospitalname,state,outcome
   list<-list[complete.cases(list), ]             ##Na cleaning
   
   hname<-list[ ,1]
   outcome<-list[ ,3]
   orderlist<-list[order(outcome,hname,decreasing=FALSE)]
   if(num=="best"){
     select<-orderlist[1, ]
     rank<-rbind(rank,select)
   }
   if(num!="best"){
     select<-orderlist[num, ]
     rank<-rbind(rank,select)
   }
  }
  
  ##minimum<-min(list[ ,3])
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

}