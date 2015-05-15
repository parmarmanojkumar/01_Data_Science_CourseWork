corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  files_list <- list.files(directory, full.names = TRUE) #creates a list of files
  id = 1: length(files_list)
  dat <- data.frame() #creates an empty data frame
  rcor <- NULL
  for (i in id) {
    # loops through the files, rbinding them together
    rcort <- NULL
    cp <- complete(directory,i)
    if (cp$nobs > threshold){
      dat <- read.csv(files_list[i])
      rcort <- cor(dat$sulfate, dat$nitrate , use = "pairwise.complete.obs")
      rcor <- rbind(rcor,rcort)
    }

  }
  return(rcor)
}