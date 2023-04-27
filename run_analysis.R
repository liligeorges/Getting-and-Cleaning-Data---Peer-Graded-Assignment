## Load library + set WD + download data

  install.packages("dplyr", "data.table")
  setwd("UCI HAR Dataset")

## Download file
  
  if (!file.exists("UCI HAR Dataset")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
  }  

  if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
  }
  
## Assign data frames
  
  features <- read.table("features.txt", col.names = c("n","functions"))
  activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
  subject_test <- read.table("test/subject_test.txt", col.names = "subject")
  subject_train <- read.table("train/subject_train.txt", col.names = "subject")
  x_test <- read.table("test/X_test.txt", col.names = features$functions)
  y_test <- read.table("test/y_test.txt", col.names = "code")
  x_train <- read.table("train/X_train.txt", col.names = features$functions)
  y_train <- read.table("train/y_train.txt", col.names = "code")

## 1. Merges the training and the test sets to create one data set.
  
  X <- rbind(x_train, x_test)
  Y <- rbind(y_train, y_test)
  SUBJECT <- rbind(subject_train, subject_test)
  mergedData <- cbind(X, Y, SUBJECT)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  
  col_MeanSTD <- grep(".*Mean.*|.*Std.*", names(mergedData), ignore.case=TRUE)
  requiredColumns <- c(col_MeanSTD, 562, 563)
  dim(mergedData)
  
  extractedData <- mergedData[,requiredColumns]
  dim(extractedData)
  
  
  ## OR
  
  
  features <- read.table("features.txt")
  mean_std_features <- grep("-(mean|std)\\(\\)", features[, 2])
  x_data <- X[, mean_std_features]
  names(X) <- features[mean_std_features, 2]


## 3. Uses descriptive activity names to name the activities in the data set.
  
  extractedData$activities <- as.character(extractedData$activities)
  
  for (i in 1:6){
    extractedData$activities[extractedData$activities == i] <- as.character(activity_labels[i,2])
  }
  
  extractedData$activities <- as.factor(extractedData$activities)
  
  ## OR
  
  activities <- read.table("activity_labels.txt")
  Y[, 1] <- activities[Y[, 1], 2]
  names(Y) <- "Activity"
  names(SUBJECT) <- "Subject"
  mergedData <- cbind(Y, SUBJECT)

## 4. Appropriately labels the data set with descriptive variable names.
  
  names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
  names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
  names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
  names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
  names(extractedData)<-gsub("^t", "Time", names(extractedData))
  names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
  names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
  names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
  names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
  names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
  names(extractedData)<-gsub("angle", "Angle", names(extractedData))
  names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
  
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

  tidyData2 <- aggregate(. ~ subject + code, extractedData, mean)
  
  write.table(tidyData2, "tidyData2.txt", row.names = FALSE)
