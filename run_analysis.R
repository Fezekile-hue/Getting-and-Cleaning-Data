#Download equired  R packages
library(dplyr)
library(data.table)

#Read Test Data
subject_test <- read.table("UCI HAR Dataset/subject_test.txt")
x_test<-read.table("UCI HAR Dataset/test/X_test.txt")
y_test<- read.table("UCI HAR Dataset/test/y_test.txt")

#Read Training Data
subject_train <- read.table("UCI HAR Dataset/subject_train.txt")
x_train<-read.table("UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")



# Merge training and test sets to create one data set
subject <- rbind(subject_train, subject_test)
features <- rbind(x_train, x_test)
activity <- rbind(y_train, y_test)

# Labeling data set
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

combinedData <- cbind(features,activity,subject)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

feature_names <- read.table("./UCI HAR Dataset/features.txt", sep=" ", comment.char = "", colClasses="character")


columnsWithMeanSTD <- grepl(".*Mean.*|.*Std.*", names(feature_names), ignore.case=TRUE)

subsetData <- combinedData[,c(columnsWithMeanSTD, 562, 563)]

# 3.  Uses descriptive activity names to name the activities in the data set

activity_label<-read.table("UCI HAR Dataset/activity_labels.txt")
activity_name<-activity_label[,2]

activity_extract<-as.character(subsetData[,1])

# substituting all the numbers in activity_extract with appropriate labels from activity_label

for(i in 1:6){
  activity_extract [activity_extract==i]<-as.character(activity_label[i,2])
  
}

activity_extract<-as.factor(activity_extract)
subsetData$Subjec <- as.factor(subsetData)
subsetData[,1]<-activity_extract

#4 Appropriately labels the data set with descriptive variable names.
names(subsetData)<-gsub("Acc", "Accelerometer", names(subsetData))
names(subsetData)<-gsub("Gyro", "Gyroscope", names(subsetData))
names(subsetData)<-gsub("BodyBody", "Body", names(subsetData))
names(subsetData)<-gsub("Mag", "Magnitude", names(subsetData))
names(subsetData)<-gsub("^t", "Time", names(subsetData))
names(subsetData)<-gsub("^f", "Frequency", names(subsetData))
names(subsetData)<-gsub("tBody", "TimeBody", names(subsetData))
names(subsetData)<-gsub("-mean()", "Mean", names(subsetData), ignore.case = TRUE)
names(subsetData)<-gsub("-std()", "STD", names(subsetData), ignore.case = TRUE)
names(subsetData)<-gsub("-freq()", "Frequency", names(subsetData), ignore.case = TRUE)
names(subsetData)<-gsub("angle", "Angle", names(subsetData))
names(subsetData)<-gsub("gravity", "Gravity", names(subsetData))

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data<-aggregate(. ~Subject + Activity, subsetData, mean)
tidy_Data <- tidy_Data[order(tidy_Data$Subject,tidy_Data$Activity),]  
write.table(tidy_data,file="tidy_data.txt",row.names=FALSE)

