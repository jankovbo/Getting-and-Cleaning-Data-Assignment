# 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('C:\\Users\\jankovbo\\Desktop\\Moje\\DataScience\\Coursera\\03_Getting_And_Cleaning_Data\\Project\\UCI HAR Dataset');

# Read in the data from files
features     = read.table("features.txt",header=FALSE) #imports features.txt
activityType = read.table("activity_labels.txt",header=FALSE)#imports activity_labels.txt
subjectTrain = read.table("train/subject_train.txt",header=FALSE) #imports subject_train.txt
xTrain       = read.table("train/X_train.txt",header=FALSE) #imports x_train.txt
yTrain       = read.table("train/Y_train.txt",header=FALSE) #imports y_train.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

# Create the final training set merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

# Read in the test data
subjectTest = read.table("test/subject_test.txt",header=FALSE) #imports subject_test.txt
xTest       = read.table("test/X_test.txt",header=FALSE) #imports x_test.txt
yTest       = read.table("test/Y_test.txt",header=FALSE) #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"


# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest)


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)

colNames  = colnames(finalData)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

finalData = finalData[mean_and_std==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)


colNames  = colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names. 

colnames <-colnames(finalData)
colnames <- make.names(colnames, unique=TRUE)

#Cleanup the variable names by replacing characters
colnamesclean<-gsub("-", " ", colnames) 
colnamesclean<-gsub("\\.", " ", colnamesclean)
colnamesclean<-gsub("\\  ", " ", colnamesclean)
colnamesclean<- gsub("\\()","",colnamesclean)
colnamesclean <- gsub("^(t)","time",colnamesclean)
colnamesclean<- gsub("^(f)","freq",colnamesclean)
colnamesclean<-gsub("BodyBody", "Body", colnamesclean)
colnamesclean<-gsub("^\\s+|\\s+$", "", colnamesclean)
colnamesclean<- gsub("-std$","StdDev",colnamesclean)
colnamesclean<-gsub("-mean","Mean",colnamesclean)
colnamesclean<- gsub("JerkMag","JerkMagnitude",colnamesclean)
colnamesclean<-gsub("GyroMag","GyroMagnitude",colnamesclean)
colnamesclean<-gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnamesclean)


colnames(finalData) = colnamesclean
str(finalData)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- aggregate(finalData[,3:81], by = list(activity = finalData$activityId, subject = finalData$subjectId),FUN = mean)
write.table(x = tidydata, file = "tidy.txt", row.names = FALSE)

