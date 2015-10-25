
# Holds the filename for the downloaded zip
fileNm <- "w3proj.zip"
# Will hold the list of files in the zip
fileLst <- ""

downloadZip <- function() {
    dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    download.file(dataUrl, fileNm, method="curl")
}

unzipFile <- function() {
    fileLst <<- unzip(fileNm)
}

runAnalysis <- function() {
    #Step 1. load activity list
    activity <- read.delim("UCI HAR Dataset/activity_labels.txt", header=FALSE,
                           sep=" ", col.names=c("Idx", "activityNm"))
    #Step 2. load features list
    features <- read.delim("UCI HAR Dataset/features.txt", header=FALSE,
                           sep=" ", col.names=c("Idx", "Variable"))
    
    ##Step3. load train dataset
    #Step3a. Load train subjects
    trainSubject <- read.delim("UCI HAR Dataset/train/subject_train.txt", header=FALSE,
                           sep=" ", col.names=c("Subject"))
    
    #Step3b. load train activity
    trainActivity <- read.delim("UCI HAR Dataset/train/y_train.txt", header=FALSE,
                           sep=" ", col.names=c("ActivityId"))
    
    #Step3c. load train activity data
    trainDat <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE,
                           col.names=features$Variable, check.names=FALSE)
    #Step3d. add subject to the dataset
    trainDat <- mutate(trainDat, subject=trainSubject$Subject,
                       activity=trainActivity$ActivityId)
    
    
    ##Step4. load test dataset
    #Step4a. Load test subjects
    testSubject <- read.delim("UCI HAR Dataset/test/subject_test.txt", header=FALSE,
                               sep=" ", col.names=c("Subject"))
    
    #Step4b. load test activity
    testActivity <- read.delim("UCI HAR Dataset/test/y_test.txt", header=FALSE,
                                sep=" ", col.names=c("ActivityId"))
    
    #Step4c. load test activity data
    testDat <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE,
                           col.names=features$Variable, check.names=FALSE)
    #Step4d. add subject to the dataset
    testDat <- mutate(testDat, subject=testSubject$Subject,
                      activity=testActivity$ActivityId)
    
    #Step5. mergeing test and train data
    mergeDat <- rbind(trainDat, testDat)
    
    #Step6. Extract only mean or std data
    meanStdDat <- mergeDat[,colnames(mergeDat)[grep("mean|std|subject|activity", colnames(mergeDat))]]
    
    #Step7. Add activity description to the dataset
    meanStdDat <- merge(meanStdDat, activity, by.x="activity", by.y="Idx")
    
    #Step8. Create tidy dataset with average of each activity & subject
    tidyDat <- ddply(meanStdDat, 
                     c("activity", "subject", "activityNm"), 
                     colwise(mean))
    
    #Step9. Write the tidy data to file as table.
    write.table(tidyDat, "tidyData.txt", row.name=FALSE)
}