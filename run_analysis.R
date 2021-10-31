
# 1. Merge the training and the test sets to create one data set.
setwd("E:/RTour/data_cleaning_week_4/run_analysis")

# Import training data and rename the columns 
Features <- read.table('./UCI HAR Dataset/features.txt',header=FALSE)

Activity_Labels <- read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)
colnames(Activity_Labels) <- c("Activity_Id","Activity_Type")
#Training Data
Subject_Train <- read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
colnames(Subject_Train) <- "Subject_Id"

X_Train <- read.table('./UCI HAR Dataset/train/X_train.txt',header=FALSE)
colnames(X_Train) <- Features[,2]

y_Train <- read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)
colnames(y_Train) <- "Activity_Id"

Training_Set = cbind(y_Train,Subject_Train,X_Train)
#Test Data
Subject_Test <- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
colnames(Subject_Test) <- "Subject_Id"

X_Test <- read.table('./UCI HAR Dataset/test/X_test.txt',header=FALSE)
colnames(X_Test) <- Features[,2]

y_Test <- read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)
colnames(y_Test) <- "Activity_Id"

Test_Set = cbind(y_Test,Subject_Test,X_Test)

# Combine Training Data and Test Data into one Merged Data Set
Merged_Data_Set = rbind(Training_Set,Test_Set)

#columns variety for columns names process.
columns <- colnames(Merged_Data_Set)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

# Create a Condition vector for fill in filter condition.
Conditions <- (grepl("activity..",columns,ignore.case = TRUE) 
                |grepl("subject..",columns,ignore.case = TRUE) 
                |grepl("mean\\(\\)",columns,ignore.case = TRUE) 
                |grepl("std\\(\\)",columns,ignore.case = TRUE))


Merged_Data_Set <- Merged_Data_Set[Conditions==TRUE]


# 3. Use descriptive activity names to name the activities in the data set


Merged_Data_Set <- merge(Merged_Data_Set,Activity_Labels,
                         by='Activity_Id',all.x=TRUE);
Merged_Data_Set$Activity_Id <-Activity_Labels[,2][match(Merged_Data_Set$Activity_Id, Activity_Labels[,1])] 

columns <- colnames(Merged_Data_Set)

#4.Appropriately labels the data set with descriptive variable names. 
names(Merged_Data_Set)<-gsub("^t", "time", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("^f", "frequency", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("Acc", "Accelerometer", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("Gyro", "Gyroscope", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("Mag", "Magnitude", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("BodyBody", "Body", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("\\(", "", names(Merged_Data_Set))
names(Merged_Data_Set)<-gsub("\\)", "", names(Merged_Data_Set))

#5.From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
Second_Data_Set<-aggregate(Merged_Data_Set[,names(Merged_Data_Set) != c('activityId','subjectId')],
                           by=list(Activity_Id=Merged_Data_Set$Activity_Id,
                                   Subject_Id=Merged_Data_Set$Subject_Id),mean);


write.table(Second_Data_Set, "Tidy Data Set.txt", row.names = FALSE)