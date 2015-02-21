# See README.md for general information on this script and CodeBook.md
# for information on the output variables included in the tidy data set.

# Build folder and file paths
dataFolderPath <- "UCI HAR Dataset"
featuresFilePath <- paste(dataFolderPath, "features.txt", sep = "/")
activityLabelsFilePath <- paste(dataFolderPath, "activity_labels.txt",
                                sep = "/")
testFolderPath <- paste(dataFolderPath, "test", sep = "/")
trainFolderPath <- paste(dataFolderPath, "train", sep = "/")
testSubjectFilePath <- paste(testFolderPath, "subject_test.txt", sep = "/")
trainSubjectFilePath <- paste(trainFolderPath, "subject_train.txt",
                              sep = "/")
testActivityFilePath <- paste(testFolderPath, "y_test.txt", sep = "/")
trainActivityFilePath <- paste(trainFolderPath, "y_train.txt", sep = "/")
testFeatureFilePath <- paste(testFolderPath, "x_test.txt", sep = "/")
trainFeatureFilePath <- paste(trainFolderPath, "x_train.txt", sep = "/")

# Import test and training subject data
testSubject <- read.table(testSubjectFilePath)
trainSubject <- read.table(trainSubjectFilePath)

# Label subject column name
colnames(testSubject) <- "subject"
colnames(trainSubject) <- "subject"

# Import test and training activity data
testActivity <- read.table(testActivityFilePath)
trainActivity <- read.table(trainActivityFilePath)

# Label activity column name
colnames(testActivity) <- "act_id"
colnames(trainActivity) <- "act_id"

# Import test and training feature data
testFeature <- read.table(testFeatureFilePath)
trainFeature <- read.table(trainFeatureFilePath)

# Import feature names
features <- read.table(featuresFilePath)

# Label feature columns
colnames(testFeature) <- features[, 2]
colnames(trainFeature) <- features[, 2]

# Merge training set components into one training set and test set
# components into one test set
trainingData <- cbind(trainSubject, trainActivity, trainFeature)
testData <- cbind(testSubject, testActivity, testFeature)

# 1. Merge the training and the test sets to create one data set.
mergeData <- rbind(trainingData, testData)

# 2. Extract only the measurements on the mean and standard deviation
#    for each measurement.

# Find features with "mean", "std", "act_id", or "subject" in feature name
filterFeatures <- grep("mean|std|act_id|subject", colnames(mergeData))

# Select only the desired features
filterData <- subset(mergeData, select = filterFeatures)

# 3. Use descriptive activity names to name the activities in the data set.

# Import activity name list
activityLabels <- read.table(activityLabelsFilePath)

# Rename columns of activity list
colnames(activityLabels) <- c("id", "activity")

# Perform join operation to match activity names to respective activity id's
filterData <- merge(filterData, activityLabels, by.x = "act_id", by.y = "id")

# Remove act_id column since it is no longer needed
filterData$act_id <- NULL

# Move activity column to second column position
columnCount <- ncol(filterData)
columnCountMinus1 <- columnCount - 1
filterData <- filterData[c(1, columnCount, 2:columnCountMinus1)]

# 4. Appropriately label the data set with descriptive variable names.

# Remove hyphens and parentheses as well as convert variable names to
# lowerCamelCase due to variable name lengths
colnames(filterData) <- gsub("-m", "M", colnames(filterData))
colnames(filterData) <- gsub("-s", "S", colnames(filterData))
colnames(filterData) <- gsub("-|\\()", "", colnames(filterData))

# Convert non-obvious abbreviations to a longer form for readability
colnames(filterData) <- gsub("tBo", "timeBo", colnames(filterData))
colnames(filterData) <- gsub("tGr", "timeGr", colnames(filterData))
colnames(filterData) <- gsub("fBo", "freqBo", colnames(filterData))
colnames(filterData) <- gsub("fGr", "freqGr", colnames(filterData))
colnames(filterData) <- gsub("Acc", "Acceleration", colnames(filterData))
colnames(filterData) <- gsub("Gyro", "Gyroscope", colnames(filterData))
colnames(filterData) <- gsub("Mag", "Magnitude", colnames(filterData))
colnames(filterData) <- gsub("Std", "StdDev", colnames(filterData))

# 5. Create a second, independent tidy data set with the average of each
#    variable for each activity and each subject.

# Specify grouping elements
groupingElements <- list(activity = filterData$activity,
                         subject = filterData$subject)

# Create data set with mean of each variable for each activity and subject
# Exclude first two columns from aggregate input to prevent R from trying
# to average the activity and subject columns
tidyData = aggregate(filterData[3:columnCount], by = groupingElements, mean)

# Create text file of tidy data set
write.table(tidyData, "tidyData.txt", row.name = FALSE)
