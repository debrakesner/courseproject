#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
library(reshape2)

#load descriptive tables
features <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/features.txt")
activitylabels <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/activity_labels.txt")

#training set
trainingset <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/train/X_train.txt")
traininglabels <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/train/y_train.txt")
trainingsubject <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt")

#testset
testset <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/test/X_test.txt")
testlabels <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/test/y_test.txt")
testsubject <- read.table("C:/Users/Debra/datasciencecoursera/UCI HAR Dataset/test/subject_test.txt")

#label feature columns 
featurem <- cbind(features,features[,2])
colnames(featurem) <- c("columnid","featureraw","featurepretty")
featurem[,3] <- tolower(featurem[,3])
featurem[,3] <- sub("^t","time", featurem[,3])
featurem[,3] <- sub("^f","frequency", featurem[,3])
featurem[,3] <- sub("acc","accelerometer", featurem[,3])
featurem[,3] <- sub("gyro","gyroscope", featurem[,3])
featurem[,3] <- sub("mag","magnitude", featurem[,3])
featurem[,3] <- sub("\\()","", featurem[,3])
featurem[,3] <- gsub("\\-","", featurem[,3])
featurem[,3] <- sub("x$","xaxis", featurem[,3])
featurem[,3] <- sub("y$","yaxis", featurem[,3])
featurem[,3] <- sub("z$","zaxis", featurem[,3])
featurem[,3] <- sub("^angle\\(","anglebtwn", featurem[,3])
featurem[,3] <- sub("anglebtwn+x","anglebtwnxaxis", featurem[,3])
featurem[,3] <- sub("anglebtwn+y","anglebtwnyaxis", featurem[,3])
featurem[,3] <- sub("anglebtwnf+z","anglebtwnzaxis", featurem[,3])
featurem[,3] <- gsub("\\)","", featurem[,3])
featurem[,3] <- gsub("\\,","to", featurem[,3])

#label columns of various data sets
colnames(activitylabels) <- c("label","activity") 

#combine subjects with test labels
colnames(testset) <- featurem[,3]
testdata <- cbind(testsubject,testlabels)
colnames(testdata) <- c("subject","label")
testdata <- merge(testdata,activitylabels, by.x = "label", by.y = "label")
testdata <- cbind(testdata,testset)
testdata$trainortest <- c("test")

#combine subjects with train labels
colnames(trainingset) <- featurem[,3]
trainingdata <- cbind(trainingsubject,traininglabels)
colnames(trainingdata) <- c("subject","label")
trainingdata <- merge(trainingdata,activitylabels, by.x = "label", by.y = "label")
trainingdata <- cbind(trainingdata,trainingset)
trainingdata$trainortest <- c("train")

#union test and train
dataAll <- rbind(trainingdata,testdata)

#return only mean and standard deviation colunns
dataMeanStd <- dataAll[,grep("mean|std",names(dataAll))]
dataMeanStd <- cbind(dataAll[,2:3],dataAll$trainortest,dataMeanStd)
names(dataMeanStd)[3] <- "trainortest"

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
meltdata <- melt(dataMeanStd,id = c(names(dataMeanStd)[1:3]), measure.vars = c(names(dataMeanStd)[4:89]), value.name = "featurevalue")
avgdata <- dcast(meltdata,subject + activity + trainortest ~ variable,mean)
names(avgdata)[4:89] <- paste0("avg",names(avgdata)[4:89])
write.table(avgdata,file = "averagedata.txt",row.name=FALSE)

