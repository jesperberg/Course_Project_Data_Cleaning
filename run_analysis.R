## Create directories and download the required data.
## I have pre-downloaded the data, and unzipped it manually.

setwd("D:/Program/RStudio/Projects/Coursera/Data cleaning/Week 4/Course_Project_Min/data/UCI HAR Dataset")

## Begin importing all the required files from the unzipped file.

xTrain <- read.table("./train/x_train.txt", header = FALSE)
yTrain <- read.table("./train/y_train.txt", header = FALSE)
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
activityType <- read.table("./activity_labels.txt", header = FALSE)
features <- read.table("./features.txt", header = FALSE)

## After importing the tables, begin creating the correct column names to the files.

colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"
colnames(subjectTrain) <- "subjectId"
colnames(activityType) <- c("activityId","activityType")

## After column names has been defined, create a final training set that are to be used.
## This is done by merging all the training datasets.

trainingDataset <- cbind(yTrain, subjectTrain, xTrain)

## After trainingDataset has been created, start importing the provided test data.

xTest <- read.table("./test/x_test.txt", header = FALSE)
yTest <- read.table("./test/y_test.txt", header = FALSE)
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)


## As for the training dataset, create the correct column names for test data.

colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"
colnames(subjectTest) <- "subjectId"
colnames(activityType) <- c("activityId","activityType")


## When correct column names has been created, merge the test datasets together into a final one.

testDataset <- cbind(yTest, subjectTest, xTest)

## When testDataset has been created, merge the training AND test dataset into one to be used

finalDataset <- rbind(trainingDataset, testDataset)

## In order to call to column names in the finalDataset 
columnnames <- colnames(finalDataset)

## 2. Now we will extract only the mean and standard deviations measurements from finalDataset.
## By creating a logical vector, we can easily create the vector showing which columns we want to extract from the dataset.

vector_logical <- (grepl("activity..",columnnames) | grepl("subject..",columnnames) | grepl("-mean..",columnnames) & !grepl("-meanFreq..",columnnames) & !grepl("mean..-",columnnames) | grepl("-std..",columnnames) & !grepl("-std()..-",columnnames)) 

## Extract the columns and observation where the vector_logical is true.

finalDataset <- finalDataset[vector_logical == TRUE]

## 3. When we have extracted the meaningful measurements (mean and std), merge the finalDataset with 
## activityType table.

finalDataset <- merge(finalDataset, activityType, by="activityId", all.x = TRUE)

## 4. Now we will clean the columnnames and make them more descriptive.
## We do this with a for-loop, looping through each column and searching for a specific word and replacing it.

for(i in 1:length(columnnames))
{
        columnnames[i] <- gsub("tBodyAccMag-mean()","TimeBodyAccMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("tBodyAccMag-std()","TimeBodyAccMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("tGravityAccMag-mean()","TimeGravityAccMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("tGravityAccMag-std()","TimeGravityAccMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("tBodyAccJerkMag-mean()","TimeBodyAccJerkMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("tBodyAccJerkMag-std()","TimeBodyAccJerkMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("tBodyGyroMag-mean()","TimeBodyGyroMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("tBodyGyroMag-std()","TimeBodyGyroMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("tBodyGyroJerkMag-mean()","TimeBodyGyroJerkMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("tBodyGyroJerkMag-std()","TimeBodyGyroJerkMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("fBodyAccMag-mean()","FreqBodyAccMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("fBodyAccMag-std()","FreqBodyAccMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("fBodyBodyAccJerkMag-mean()","FreqBodyAccJerkMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("fBodyBodyAccJerkMag-std()","FreqBodyAccJerkMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("fBodyBodyGyroMag-mean()","FreqBodyGyroMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("fBodyBodyGyroMag-std()","FreqBodyGyroMagnitude-StandardDeviation",columnnames[i])
        columnnames[i] <- gsub("fBodyBodyGyroJerkMag-mean()","FreqBodyGyroJerkMagnitude-Mean",columnnames[i])
        columnnames[i] <- gsub("fBodyBodyGyroJerkMag-mean()","FreqBodyGyroJerkMagnitude-StandardDeviation",columnnames[i])
        
}

## From above For-loop, we replace the current column names with our cleaned ones.

colnames(finalDataset) <- columnnames

## 5. Create the final tidy dataset, with average of measurements per activity type and subject. 

finalDatasetWithoutActivitytype <- finalDataset[, colnames(finalDataset) != "activityType"]

## Aggregate the dataset without activity type in order to get meaningful average.

dataset_clean <- aggregate(finalDatasetWithoutActivitytype[,names(finalDatasetWithoutActivitytype) != c('activityId','subjectId')],by=list(activityId=finalDatasetWithoutActivitytype$activityId,subjectId = finalDatasetWithoutActivitytype$subjectId),mean)

## Merge back with activityType to get activityType values back.

dataset_clean <- merge(dataset_clean, activityType,"activityId", all.x = TRUE)

## Now when we have the final and cleaned table, export it in .txt-format.

write.table(dataset_clean, './dataset_clean.txt',row.names=TRUE,sep='\t')


