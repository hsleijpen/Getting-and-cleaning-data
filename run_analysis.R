# This part of the code reads the appropriate datasets into R
setwd("./Desktop/Coursera R/")
trainingData = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
trainingData[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
trainingData[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

testData = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testData[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testData[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

# This part of the code reads the features file and makes the relevant feature names better suited
# for manipulating in R with an appropriate substitution
features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# This part of the code merges the trainingdata and testdata sets 
allData = rbind(trainingData, testData)

# This part of the code collects the required data on mean and std. dev., reduces the features table 
# into desired state and reconfigures the merged dataset adding relevant new columns and removing 
# unnecessary columns
# Collecting the required data (columns)
colsRequired <- grep(".*Mean.*|.*Std.*", features[,2])
# Reducing the features table into desired state
features <- features[colsRequired,]
# Adding relevant new (last two) columns (subject and activity)
colsRequired <- c(colsRequired, 562, 563)
# Removing the unwanted columns from allData
allData <- allData[,colsRequired]
# Adding the new column names (features) to the merged dataset
colnames(allData) <- c(features$V2, "Activity", "Subject")
colnames(allData) <- tolower(colnames(allData))

# This part of the code tidies up the reconfigured merged dataset and creates the tidy dataset
# Tidying up the reconfigured merged dataset
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
        allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
        currentActivity <- currentActivity + 1
}

allData$activity <- as.factor(allData$activity)
allData$subject <- as.factor(allData$subject)
# Creating the tidy dataset
tidyDataset = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)
# This part of the code removes the subject and activity column, since a mean of those variables has 
#no meaning
tidyDataset[,90] = NULL
tidyDataset[,89] = NULL
# This part of the code writes the tidyDataset in an "txt"-format
write.table(tidyDataset, "tidy.txt", sep="\t", row.names = FALSE)