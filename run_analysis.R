### Submission for course project for "Getting and Cleaning Data":
###     http://class.coursera.org/getdata-016/
### Description: Prepare tidy data.
### Author: sueyic@gmail.com

library(plyr)
library(reshape2)
library(stringr)
library(Hmisc)
library(dplyr)

### (1) Merge the training and the test sets to create one data set.

### Sanity check -- verify that X_test.txt and X_train.txt have the same number of columns.
testNumCols <- ncol(read.table("data/test/X_test.txt", nrow=1))     # Returns 561
trainNumCols <- ncol(read.table("data/train/X_train.txt", nrow=1))  # Returns 561
print(paste("Num cols in X_test.txt:", testNumCols, ", Num cols in X_train.txt:",  trainNumCols))
if (testNumCols != trainNumCols) {
  print(paste("Failed assertion: (testNumCols != testNumRows)"))  
  quit(status=1)
}

# Build raw_train from subject_train.txt, X_train.txt (measures), and y_train.txt (activities).
subject_train <- read.table("data/train/subject_train.txt", col.names=c("subject"))
y_train <- read.table("data/train/y_train.txt", col.names=c("activity"))
X_train <- read.table("data/train/X_train.txt")
raw_train <- cbind(subject_train, y_train, X_train)

# Build raw_test from subject_test.txt, X_test.txt (measures), and y_test.txt (activities).
subject_test <- read.table("data/test/subject_test.txt", col.names=c("subject"))
y_test <- read.table("data/test/y_test.txt", col.names=c("activity"))
X_test <- read.table("data/test/X_test.txt")
raw_test <- cbind(subject_test, y_test, X_test)

# Create the merged data set from raw_train and raw_test.
raw <- rbind(raw_train, raw_test)


### (2) Extract only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("data/features.txt", col.names=c("colIndex", "featureRawName"))
# Only keep measurements on the mean and standard deviation for each measurement.
baseNames <- c("tBodyAcc-XYZ", "tGravityAcc-XYZ", "tBodyAccJerk-XYZ", "tBodyGyro-XYZ", "tBodyGyroJerk-XYZ", "tBodyAccMag", "tGravityAccMag", "tBodyAccJerkMag", "tBodyGyroMag", "tBodyGyroJerkMag", "fBodyAcc-XYZ", "fBodyAccJerk-XYZ", "fBodyGyro-XYZ", "fBodyAccMag", "fBodyAccJerkMag", "fBodyGyroMag", "fBodyGyroJerkMag")
featureNamesToKeep <- NULL
for (baseName in baseNames) {
  if (grepl("-XYZ$", baseName)) {
    pattern <- str_extract(baseName, "^[^-]+")
    featureNamesToKeep <- append(featureNamesToKeep, paste(pattern, "-mean()-X", sep=""))
    featureNamesToKeep <- append(featureNamesToKeep, paste(pattern, "-mean()-Y", sep=""))
    featureNamesToKeep <- append(featureNamesToKeep, paste(pattern, "-mean()-Z", sep=""))
    featureNamesToKeep <- append(featureNamesToKeep, paste(pattern, "-std()-X", sep=""))
    featureNamesToKeep <- append(featureNamesToKeep, paste(pattern, "-std()-Y", sep=""))
    featureNamesToKeep <- append(featureNamesToKeep, paste(pattern, "-std()-Z", sep=""))
  }
  featureNamesToKeep <- append(featureNamesToKeep, paste(baseName, "-mean()", sep=""))
  featureNamesToKeep <- append(featureNamesToKeep, paste(baseName, "-std()", sep=""))
}
filteredFeatures <- features[features$featureRawName %in% featureNamesToKeep,]  
# This prints "Number of measurements to keep: 60
print(paste("Number of measurements to keep:", nrow(filteredFeatures)))
# Create raw2, which only has measurements on the mean and standard deviation for each measurement.
raw2 <- raw[, c("subject", "activity", ldply(filteredFeatures[,1], function(x) { paste("V", x, sep="") } )[,1])]


### (3) Uses descriptive activity names to name the activities in the data set.
raw3 <- mutate(raw2, activity=factor(raw2$activity, labels=c("walking", "walkingUpstairs", "walkingDownstairs", "sitting", "standing", "laying")))


### (4) Appropriately labels the data set with descriptive variable names. 
# Here we will rename the features.
featureRename <- function(x) {
  # "-" is an invalid character of a column name in R, so we will replace it.
  newName <- gsub("\\-", "", x)
  newName <- gsub("mean\\(\\)", "Mean", newName)
  newName <- gsub("std\\(\\)", "Std", newName)
  newName
}
newFeatureNames <- vapply(filteredFeatures[,2], featureRename, "")
names(raw3) <- c("subject", "activity", newFeatureNames)

# raw3 is almost a tidy data set. To make it tidy, we will melt the columns representing
# features into a "feature" variable.
tidy <- melt(raw3, id=c("subject", "activity"), measure.vars=newFeatureNames)


### (5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
grouped <- group_by(tidy, subject, activity, variable)
tidySummarized <- summarise(grouped, mean=mean(value))

# Write tidySummarized to data/tidySummarized.txt
write.table(tidySummarized, "data/tidySummarized.txt")


### The end.
