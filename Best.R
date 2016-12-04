## This function returns the best performing hospital in a named
## state for a named morbidity (heart attack, heart failure or pneumonia)

best <- function(state,outcome){

library(dplyr)

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #read outcome data to 'data'


# Grab a subdset of data for the relevant outcomes
if (outcome == "heart attack") {
  top <- select(data, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  hospitals <- top[top$State==state & top$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  best_hosp <- hospitals$Hospital.Name[hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]       
  ordered <- sort(best_hosp)
  best_hospital <- ordered[1]
} 
else if (outcome == "heart failure") {
  top <- select(data, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  hospitals <- top[top$State==state & top$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  best_hosp <- hospitals$Hospital.Name[hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]       
  ordered <- sort(best_hosp)
  best_hospital <- ordered[1]
                
} 
else if (outcome == "pneumonia") {
  top <- select(data, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  hospitals <- top[top$State==state & top$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  best_hosp <- hospitals$Hospital.Name[hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]       
  ordered <- sort(best_hosp)
  best_hospital <- ordered[1]
                
} else {
  stop("Invalid Outcome")
}

# filter by selected state
#dims <- dim(hosp)

#if (dims[1] == 0){    # check selected state is valid
#  stop("Invalid State")
#}

best_hospital

}