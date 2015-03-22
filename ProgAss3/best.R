best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
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

#   stateOutcome = as.numeric(data[data$State==state,colOutcome])
  state = data[data$State==state,]

  # check state
  if(nrow(state) == 0){
    stop('invalid state')
  }

  # order by alphabetic order
  orderState = state[order(state[,2]),]


  #find the best rows   
  best = which.min(orderState[,colOutcome])


  orderState[best,2]

  
}
