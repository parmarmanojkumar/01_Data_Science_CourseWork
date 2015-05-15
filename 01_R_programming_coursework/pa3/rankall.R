# Write a function called rankall that takes two arguments: an outcome name
# (outcome) and a hospital rank- ing (num). The function reads the 
# outcome-of-care-measures.csv file and returns a 2-column data frame 
# containing the hospital in each state that has the ranking specified in num.
# For example the function call rankall("heart attack", "best") would return 
# a data frame containing the names of the hospitals that are the best in their
# respective states for 30-day heart attack death rates. The function should 
# return a value for every state (some may be NA). The first column in the data
# frame is named hospital, which contains the hospital name, and the second 
# column is named state, which contains the 2-character abbreviation for the 
# state name. Hospitals that do not have data on a particular outcome should 
# be excluded from the set of hospitals when deciding the rankings.
# 
# Handling ties. The rankall function should handle ties in the 30-day 
# mortality rates in the same way that the rankhospital function handles ties.
# 
# NOTE: For the purpose of this part of the assignment (and for efficiency), 
# your function should NOT call the rankhospital function from the previous 
# section.
# 
# The function should check the validity of its arguments. If an invalid
# outcome value is passed to rankall, the function should throw an error 
# via the stop function with the exact message “invalid outcome”. The num 
# variable can take values “best”, “worst”, or an integer indicating the 
# ranking (smaller numbers are better). If the number given by num is larger
# than the number of hospitals in that state, then the function should return NA.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        diesease_arg <- outcome
        num_arg <- num
        list_files = list.files("rprog-data-ProgAssignment3-data",full.names = T)
        hospital_care <- read.csv(list_files[3],colClasses = "character")
        hospital_care$State <- as.factor(hospital_care$State)
        #argument_list
        diesease_arg_list = c("heart attack","heart failure","pneumonia" )
        state_arg_list = levels(hospital_care$State)
        #check for arguments
        
        if (!any(diesease_arg_list == diesease_arg)){
                stop("invalid outcome")
        }
        #subset the data frame
        #hospital_state <- hospital_care
        idx = 11 + (which(diesease_arg_list == diesease_arg) - 1) * 6
        colnames(hospital_care)[idx] <- "doi"
        hospital_care$doi<- suppressWarnings(as.numeric(hospital_care$doi))
        hospital_state <- split(hospital_care, hospital_care$State)
        rank_local <- function(x){
                order.hospital <- order(x$doi, x$Hospital.Name, na.last = NA)
                if (num_arg == "worst"){
                        idxh = length(order.hospital)
                } else if(num_arg == "best") {
                        idxh = 1
                } else {
                        idxh = num_arg
                }
                if (idxh > length(order.hospital)){
                        h_name <- NA
                }else{
                        h_name <- x$Hospital.Name[order.hospital[idxh]]
                }
                as.character(h_name)
        }
        
        df <- data.frame( sapply(hospital_state,rank_local) ,(state_arg_list), stringsAsFactors=FALSE)
        colnames(df) <- c("hospital", "state")
        df
}