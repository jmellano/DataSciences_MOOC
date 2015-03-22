rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  #check outcome
  colOutcome = 0
  switch(outcome, 
         'heart attack'={
           colOutcome = 11
         },
         'heart failure'={
           colOutcome = 17
         },
         'pneumonia'={
           colOutcome = 23
         },
          {
            stop('invalid outcome')
          }
      )
    
    # Get List of outcome filter by state
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    state = data[data$State==state,]
    # check state
    if(nrow(state) == 0){
      stop('invalid state')
    }
    
    # order by alphabetic order
    orderState = state[order(state[,2]),]
    # then by result
    orderState = orderState[order(as.numeric(orderState[,colOutcome])),]
    # clean data
    orderState = orderState[!is.na(as.numeric(orderState[,colOutcome])),]
    
    #find the best rows  
  if(num == "best"){
    best = which.min(orderState[,colOutcome])
  }else if(num == "worst"){
    best = which.max(orderState[,colOutcome])
  }else{
    best = num
  }
    
  orderState[best,2]
}
