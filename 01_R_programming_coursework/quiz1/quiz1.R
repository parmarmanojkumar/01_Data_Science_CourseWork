
#set working directory
setwd("~/Documents/01_Courses/11_R_programming/03_assignments_quiz/quiz1")
#read data file
hw_data <- read.csv("hw1_data.csv")
#extract first 2 row of data frame
hw_data[c(1,2),]
head(hw_data)
#no. of columns
nrow(hw_data)
#extract last 2 rows of data frame
hw_data[c(nrow(hw_data)-1, nrow(hw_data)),]
tail(hw_data)
#read 47th element of ozone
hw_data[[1]][47]
hw_data$Ozone[47]

#no. of missing NA values in Ozone column
sum(is.na(hw_data[[1]]))
sum(is.na(hw_data$Ozone))
#mean value  of ozone without na
mean(hw_data[[1]], na.rm = TRUE)
mean(hw_data$Ozone, na.rm = TRUE)
# mean of Solar.R where Ozone > 31 and Temp > 90
mean(hw_data[c(hw_data$Ozone > 31 & !is.na(hw_data$Ozone) & hw_data$Temp >90),]$Solar.R)
tapply(hw_data$Solar.R, hw_data$Ozone > 31 & !is.na(hw_data$Ozone) & hw_data$Temp >90,mean)
tapply(hw_data$Solar.R, hw_data$Ozone > 31 & hw_data$Temp >90,mean, na.rm = TRUE)
#mean of Temp during month is 6
mean(hw_data[c(hw_data$Month == 6),]$Temp)
tapply(hw_data$Temp, hw_data$Month ==6, mean)
#max ozone value in month of may (5)
max(hw_data[c(hw_data$Month == 5), ]$Ozone, na.rm = TRUE)
tapply(hw_data$Ozone,hw_data$Month == 5, max, na.rm = TRUE)
#reset the working directory
rm(hw_data)

