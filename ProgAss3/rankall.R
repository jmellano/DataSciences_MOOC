rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name


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
    data <- data.frame(read.csv("outcome-of-care-measures.csv", colClasses = "character"))

    states = split(data, data$State)
    nbstates = length(names(states))
    rank <- c()
    for(i in 1:nbstates){
      state = states[[i]]
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
      
      rank = rbind(rank,c(orderState[best,2],state[1,7]))
#       rank = rbind(rank,c(state[,7], orderState[best,2]))
      
    }
    rank = data.frame(rank)
    colnames(rank) <- c("hospital", "state")
    rank 
}
