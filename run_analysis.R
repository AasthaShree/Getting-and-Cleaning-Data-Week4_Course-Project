library(dplyr)

#Download the dataset from the UCI source given to the working directory

if(!file.exists("./UCIdataset.zip"))
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, "./UCIdataset.zip", method = "curl")

#Unzip the file to the set path and check whether extracted files exist
  
  dir.create("./UCIdataset")
  unzip(zipfile = "./UCIdataset.zip", exdir = "./UCIdataset")
  list.files(file.path("./UCIdataset"), recursive = TRUE)
  
#Read train and test data 
  #Read training data
  xtrain <- read.table("./UCIdataset/UCI HAR Dataset/train/X_train.txt")
  ytrain <- read.table("./UCIdataset/UCI HAR Dataset/train/Y_train.txt")
  train_subj <- read.table("./UCIdataset/UCI HAR Dataset/train/subject_train.txt")
  
  #Read testing data
  xtest <- read.table("./UCIdataset/UCI HAR Dataset/test/X_test.txt")
  ytest <- read.table("./UCIdataset/UCI HAR Dataset/test/Y_test.txt")
  test_subj <- read.table("./UCIdataset/UCI HAR Dataset/test/subject_test.txt")
  
  #Check the structure/properties of the extracted data tables
  str(xtrain)
  str(ytrain)
  str(train_subj)
  str(xtest)
  str(ytest)
  str(test_subj)
  
#Read the additional features and activity files 
  #Label the columns
  
  features <- read.table("./UCIdataset/UCI HAR Dataset/features.txt", col.names = c("FeatureId", "Features Captured"))
  activity_lbl <- read.table("./UCIdataset/UCI HAR Dataset/activity_labels.txt", col.names = c("ActivityId", "Activity"))
  
  
#1.Merges the training and the test sets to create one data set.
  
  #Bind the data tables by Row 
  
  ##Training and Test data set
  Dataset <- rbind(xtrain, xtest) 
  
  ##Training and Test activity labels
  Activityset <- rbind(ytrain, ytest) 
  
  ##Training and Test subject id(range 1-30)
  Subjectset <- rbind(train_subj, test_subj)    
  
  #Define variable names
  colnames(Dataset) <- features[,2]
  colnames(Activityset) <- "ActivityId"
  colnames(Subjectset) <- "Subject"
  
  #Merge by Column to create the required single data set
  TotalData <- cbind(Activityset, Subjectset, Dataset)
  
  #Check structure/property of data set
  str(TotalData)
  
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
 
  specific_features <- grep(".*mean.*|.*std.*|^ActivityId$|^Subject$", colnames(TotalData))
  TotalData_label <- TotalData[specific_features]

  #Check structure/property of the required data set
  str(TotalData_label)
  
#3.Uses descriptive activity names to name the activities in the data set
  
  FinalDataset <- merge(TotalData_label, activity_lbl, by = 'ActivityId', all.x = TRUE)
  
  #Check whether variables for Subject and ActivityId are labeled
  names(FinalDataset)  

#4.Appropriately labels the data set with descriptive variable names
  
  #Label other features with descriptive names
  names(FinalDataset) <- gsub("^t","Timebased_",names(FinalDataset))
  names(FinalDataset) <- gsub("^f","Frequencybased_",names(FinalDataset))
  names(FinalDataset) <- gsub("Acc","Acceleration",names(FinalDataset))
  names(FinalDataset) <- gsub("Gyro","AngularVelocity",names(FinalDataset))
  names(FinalDataset) <- gsub("Mag","Magnitude",names(FinalDataset))
  names(FinalDataset) <- gsub("BodyBody","Body",names(FinalDataset))
  names(FinalDataset) <- gsub("-mean","_Mean",names(FinalDataset))
  names(FinalDataset) <- gsub("-std","_StandardDeviation",names(FinalDataset))
  names(FinalDataset) <- gsub('[-()]', '', names(FinalDataset))
  
  #Check all the variable labels
  names(FinalDataset)
  
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
 
  TidyDataSet <- aggregate(. ~ActivityId + Subject, FinalDataset, mean)
  TidyDataSet <- TidyDataSet[order(TidyDataSet$Subject,TidyDataSet$ActivityId),]
  
  #Write second, independent tidy data in a text file
  write.table(TidyDataSet, file = "./UCIdataset/TidyDataset.txt", row.name=FALSE)
  