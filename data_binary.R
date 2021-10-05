#--------------------------------------------------------------------------------------------------------------*
#------------------------------------------ Select desired data set -------------------------------------------*
#--------------------------------------------------------------------------------------------------------------*

########################################### smoking cessation data #############################################

#setwd("~/path-to-your-dir")
current_data <- read.csv("smoking_data.csv")
colnames(current_data) <- c("study", "treatment", "responders", "sampleSize")
current_data_ref <- "No_contract"
colnames(current_data) <- c("study", "t", "responders", "sampleSize")

######################################## NSCLC data (class level) ###############################################

# setwd("~/path-to-your-dir")
# OBj_R_data=read.csv('OBj_R_data.csv', header = TRUE, sep = ",")
# current_data=OBj_R_data
# current_data_ref=  "Targeted"
# colnames(current_data)=c("study","t","responders","sampleSize")
