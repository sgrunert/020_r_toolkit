###################################################
#Author: S. Grunert
#Create: 14December2014
#Description:
#The following is for Coursera Course: getdata-016: Project 1
#This script merges training and test data from:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
###################################################

#Initialize data directory.
if(!file.exists("./projectdata")) {dir.create("./projectdata")}


#Download and unzip raw data file.
fileUrl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./projectdata/datadwnld.zip")
unzip("./projectdata/datadwnld.zip",exdir = "./projectdata")


#Read in and cbind subject and activity information from raw data.
trainLabels<-read.table("./projectdata/UCI HAR Dataset/train/y_train.txt")
testLabels<-read.table("./projectdata/UCI HAR Dataset/test/y_test.txt")

trainSubject<-read.table("./projectdata/UCI HAR Dataset/train/subject_train.txt")
testSubject<-read.table("./projectdata/UCI HAR Dataset/test/subject_test.txt")

trainRows<-cbind(trainLabels,trainSubject)
testRows<-cbind(testLabels,testSubject)

names(trainRows)<- c("label","subject")
names(testRows)<- c("label","subject")


#Match activity labels to activity names per key provided in raw data.
trainRows$activity <- 
        ifelse(trainRows$label == 1, "WALKING",
        ifelse(trainRows$label == 2, "WALKING_UPSTAIRS",
        ifelse(trainRows$label == 3, "WALKING_DOWNSTAIR",
        ifelse(trainRows$label == 4, "SITTING",
        ifelse(trainRows$label == 5, "STANDING",
        ifelse(trainRows$label == 6, "LAYING",NA))))))

testRows$activity <- 
        ifelse(testRows$label == 1, "WALKING",
        ifelse(testRows$label == 2, "WALKING_UPSTAIRS",
        ifelse(testRows$label == 3, "WALKING_DOWNSTAIR",
        ifelse(testRows$label == 4, "SITTING",
        ifelse(testRows$label == 5, "STANDING",
        ifelse(testRows$label == 6, "LAYING",NA))))))


#Select only subject and activity name columns for use as groupings.
trainObservations <- data.frame(trainRows[,2:3])
testObservations <- data.frame(testRows[,2:3])


#Read in column names for the main raw data files.
namesTable<-read.table("./projectdata/UCI HAR Dataset/features.txt")
names<-namesTable[,2]


#Select only column names for mean() and std()
std1<-grep("std()", names ,value=TRUE)
mean1<-grep("mean()", names ,value=TRUE)
meanWrong<-grep("Freq()", names ,value=TRUE) #Removes unwanted columns.
mean2<-subset(mean1,!(mean1 %in% meanWrong))
selectCols<-c(std1,mean2)


#Read in raw training data, apply column names, 
#and select only mean() and std() columns.
trainData<-read.table("./projectdata/UCI HAR Dataset/train/X_train.txt")
names(trainData)<-names
selecttrainData<-trainData[selectCols]


#Read in raw test data, apply column names, 
# and select only mean() and std() columns.
testData<-read.table("./projectdata/UCI HAR Dataset/test/X_test.txt")
names(testData)<-names
selecttestData<-testData[selectCols]


#cbind training data with corresponding subject and activity names.
alltrainData<-cbind(trainObservations,selecttrainData)


#cbind test data with corresponding subject and activity names.
alltestData<-cbind(testObservations,selecttestData)


#rbind train and test data into a combined file.
alldetailData<-rbind(alltrainData,alltestData)


#Melt subject-activity groupings, aggregate variable means,
#and reshape data.
library(reshape2)

dataMelt <- melt(alldetailData, id=c("subject","activity"),measure.vars=selectCols)
meanData <- dcast(dataMelt, subject+activity ~ variable, mean)
reshapedData <- meanData[order(meanData$subject,meanData$activity),]

#Write data to a txt file in working directory.
write.table(reshapedData,file="./wearablestats.txt", row.name=FALSE)

#File test script.
##wearablestats<-read.table("wearablestats.txt", header = TRUE )
##str(wearablestats)
##head(wearablestats,n=1)
##tail(wearablestats,n=1)







