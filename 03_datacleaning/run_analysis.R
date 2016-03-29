# Getting and Cleaning Data Course Project

setwd("c:/Users/HModi/Celgene/Per/CourseRA/DataCleaning/Data/UCI HAR Dataset")
library(dplyr)

# 1.Merges the training and the test sets to create one data set.
x_train = read.table("train/X_train.txt")# Training Sets, actual data
x_test = read.table("test/X_test.txt") # Test sets, actual data
xFinal.data = rbind(x_train,x_test)
# END merging train and test data set------------------


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# read the features and store in vector
features = read.csv("features.txt",header = F, sep = " ")
colnames(features) = c("id", "feature")
meanIndex = grep("\\b-mean()\\b",features$feature)
subIndex = grep("\\b-std()\\b",features$feature)
index = c(meanIndex,subIndex)
index = sort(index)
sub.xFinal.data = select(xFinal.data,index)
# end extracting select measurements-------------------------------------------


# 3. Uses descriptive activity names to name the activities in the data set
y_train = read.table("train/y_train.txt") # Training Labels,
y_test = read.table("test/y_test.txt") # Test labels,
yFinal.label = rbind(y_train,y_test) # combining labels

activity = read.table("activity_labels.txt")
len = nrow(activity)

# giving each activity number to a proper label, ie. 1 <- WALKING
for(i in 1:len){
  print(paste(i," ",as.character(activity[i,2])))
  if(i != 1){
      yFinal.label$Label = gsub(i,as.character(activity[i,2]),yFinal.label$Label)
  }else{
    yFinal.label$Label = gsub(i,as.character(activity[i,2]),yFinal.label$V1)
  }
}

# finally in data set appropriately label the activity
sub.xFinal.data = cbind(yFinal.label$Label,sub.xFinal.data)


# 4. Appropriately labels the data set with descriptive variable names.
# creating vector of features name
feature = as.character(features$feature)
featureName = feature[c(index)]
colnames(sub.xFinal.data) = c("Activities",featureName)
# end labeling datasets with variable names------------------------------------

# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

# Need to label data sets first with each subject.
subject_train = read.table("train/subject_train.txt") # persons' code file randomly generated from 1:10
subject_test = read.table("test/subject_test.txt") # persons' code file randomly generated from 1:30
subjectFinal = rbind(subject_train,subject_test)
colnames(subjectFinal) = c("Subject")

# label the datasets with subject
sub.xFinal.data = cbind(subjectFinal,sub.xFinal.data)
colnames(sub.xFinal.data)

# this step creates mean of each combination of subject and activity
final.data = aggregate(sub.xFinal.data[,3:ncol(sub.xFinal.data)],
                       by=list(subject = sub.xFinal.data$Subject,
                               label = sub.xFinal.data$Activities),
                       mean)

## Final step
# write the data for course upload
write.table(format(final.data, scientific=T), "tidy.har.txt",
            row.names=F, col.names=F, quote=2)

