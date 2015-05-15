complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  rid <- NULL
  rcount <- NULL
  files_list <- list.files(directory, full.names = TRUE) #creates a list of files
  dat <- data.frame() #creates an empty data frame
  for (i in id) {
    # loops through the files, rbinding them together
    dat <- read.csv(files_list[i])
    count = 0
    len <- nrow(dat)
    rid <- rbind(rid,i)
    for (i in 1:len ){
      sum_complete <- sum(is.na(dat[i,]))
      if (sum_complete == 0){
        count = count +1
      }
    }

    rcount <- rbind(rcount,count)
  }
  rdat <- data.frame(rid,rcount, row.names = NULL)
  colnames(rdat) <- c("id","nobs")
  return(rdat)
}