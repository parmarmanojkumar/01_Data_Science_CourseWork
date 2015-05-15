# # 
# Write a function called best that take two arguments: the 2-character 
# abbreviated name of a state and an outcome name. The function reads the 
# outcome-of-care-measures.csv file and returns a character vector with the 
# name of the hospital that has the best (i.e. lowest) 30-day mortality for 
# the specified outcome in that state. The hospital name is the name provided 
# in the Hospital.Name variable. The outcomes can be one of “heart attack”, 
# “heart failure”, or “pneumonia”. Hospitals that do not have data on a 
# particular outcome should be excluded from the set of hospitals when 
# deciding the rankings.
# 
# Handling ties. If there is a tie for the best hospital for a given outcome, 
# then the hospital names should be sorted in alphabetical order and the first 
# hospital in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” 
#                                        are tied for best, then hospital 
#                                        “b” should be returned).
# 
# The function should check the validity of its arguments. If an invalid state
# value is passed to best, the function should throw an error via the stop 
# function with the exact message “invalid state”. If an invalid outcome value
# is passed to best, the function should throw an error via the stop function 
# with the exact message “invalid outcome”.

best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        state_arg <- state
        diesease_arg <- outcome
        list_files = list.files("rprog-data-ProgAssignment3-data",full.names = T)
        hospital_care <- read.csv(list_files[3],colClasses = "character")
        hospital_care$State <- as.factor(hospital_care$State)
        #argument_list
        diesease_arg_list = c("heart attack","heart failure","pneumonia" )
        col_list <- names(hospital_care)
        state_arg_list = levels(hospital_care$State)
        #check for arguments
        if (!any(state_arg_list == state_arg)){
                stop ("invalid state")
        }
        if (!any(diesease_arg_list == diesease_arg)){
                stop("invalid outcome")
        }
        #subset the data frame
        hospital_state <- subset(hospital_care, State == state_arg)
        idx = 11 + (which(diesease_arg_list == diesease_arg) - 1) * 6
        hospital_state[,idx] <- suppressWarnings(as.numeric(hospital_state[,idx]))
        #min_val <- min(hospital_state[,idx],na.rm = T)
        #idxh <- which(hospital_state[,idx] == min_val)
        #print(hospital_state$Hospital.Name[idxh])
        ans <- hospital_state$Hospital.Name[which(hospital_state[,idx] == min(hospital_state[,idx],na.rm = T))]
        ans
        
}