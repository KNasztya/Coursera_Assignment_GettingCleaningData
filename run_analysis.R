#Getting_Cleaning_Week#4 Peer-graded Assignment

# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names.
# 5.From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
-----------------------------------------------------------------------
#rm(list = ls())
  
# 1.Merges the training and the test sets to create one data set.

#1.1 Set working directory
getwd()
setwd("C:/Users/komorn/Documents/R course/03_Getting_Cleaning_Data/Week_1")

#1.2 Download and unzip files:
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="./Accelerometer_data.zip")
unzip("./Accelerometer_data.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

#1.3 Read files:
data_test  <- read.table("./UCI HAR Dataset/test/X_test.txt")
    #str(data_test)
data_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt") #Activities
    # summary(factor(y_test$V1)) 
    # 1   2   3   4   5   6 
    # 496 471 420 491 532 537 

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt") #Activities
    # summary(factor(y_train$V1)) 
    # 1    2    3    4    5    6 
    # 1226 1073  986 1286 1374 1407

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    # str(subject_test)
    # summary(factor(subject_test$V1)) # the subject who performed the activity
    # 2   4   9  10  12  13  18  20  24 
    # 302 317 288 294 320 327 364 354 381 

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
    # str(activity_labels)
    # head(activity_labels)

features <- read.table("./UCI HAR Dataset/features.txt") #Measurements' list
    # str(features)
    # head(features)

#1.4 Merge train and test sets to create one data set:

data_merged <- rbind(data_test, data_train)

y_merged <- rbind(y_test, y_train) #Activities

subject_merged <- rbind(subject_test, subject_train) #Subject who performed the activity


# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

#2.1 Label the data set with descriptive variable (measurement) names:
names(data_merged) <- features$V2
    # table(grepl("-mean()", names(data_test))) #46
    # table(grepl("-mean()", names(data_test), fixed = TRUE)) #33 fixed=T: pattern is a string
    # table(grepl("-std()", names(data_test))) #33
    # table(grepl("-mean()|-std()", names(data_test))) #79
    # table(grepl("-meanFreq()", names(data_test))) #13
    # table(grepl("-mean()",names(data_test),fixed = TRUE)|grepl("-std()",names(data_test))) #66
    # table(grepl("-mean()",names(data_merged),fixed = TRUE)|grepl("-std()",names(data_merged))) #66

#2.2 Select the extraction of only the measurements on the mean and standard deviation for each measurement:
selected_vars <- (grepl("-mean()",names(data_merged),fixed = TRUE)|grepl("-std()",names(data_merged)))
    # sum(selected_vars) #66

#2.3 Extract only the measurements on the mean and standard deviation for each measurement: 
data_merged_filt <- data_merged[,selected_vars]


# 3.Uses descriptive activity names to name the activities in the data set
#3.1 Merge data set with subject and activity
data_merged_filt_sjact <- cbind(subject_merged,y_merged,data_merged_filt)
    # str(data_merged_filt_sjact)
    # head(data_merged_filt_sjact)

#3.2 Give names to the new 2 variables:
names(data_merged_filt_sjact)[1]<-c("subject")
names(data_merged_filt_sjact)[2]<-paste("activity_label")
    # str(data_merged_filt_sjact)

#3.3 Change activity code with labeled factor:
data_merged_filt_sjact[,2] <- factor(data_merged_filt_sjact[,2], labels=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
    # str(data_merged_filt_sjact)

    # table(data_merged_filt_sjact[,2])
    # WALKING   WALKING_UPSTAIRS WALKING_DOWNSTAIRS            SITTING 
    # 1722               1544               1406               1777 
    # STANDING             LAYING 
    # 1906               1944

# 4.Appropriately labels the data set with descriptive variable names.
  Done. See:#2.1

# 5.From the data set in step 4, creates a second, 
# independent tidy data set with the average of 
# each variable for each activity and each subject.
    
 
library(dplyr) 
#5.1 Create a new dataset with the same data, but the names are different:
# - is substituded with _, and () are omitted:
data_merged_filt_sjact_v2 <- data_merged_filt_sjact
names(data_merged_filt_sjact_v2) <- gsub("-","_",names(data_merged_filt_sjact))
names(data_merged_filt_sjact_v2) <- gsub("\\()","",names(data_merged_filt_sjact_v2))

names(data_merged_filt_sjact_v2)

#5.2 Aggregate data by subject and activity in new database called dgo:
d<- data_merged_filt_sjact_v2
dg<- aggregate(d[, 3:68], list(d[,1], d[,2]), mean)
dgo <- dg[order(dg[,1]),]
head(dgo)
str(dgo)

#5.3 Fix the variable names for dgo:
names(dgo)[1]<-paste("subject")
names(dgo)[2]<-paste("activity_label")
names(dgo)[3:68]<-paste0("Mean_",names(dgo[,3:68]))

  # head(dgo)
  # str(dgo)

#5.4 Write the final tidy dataset (dgo):
setwd("C:/Users/komorn/Documents/R course/03_Getting_Cleaning_Data/Week_4_Assign")
write.table(dgo,file="./Assign_tidy_dataset.txt" , row.names=FALSE)

#Check the written final tidy dataset:
data_check <- read.table("./Assign_tidy_dataset.txt", header = TRUE) #if they used some other way of saving the file than a default write.table, this step will be different
View(data_check)
