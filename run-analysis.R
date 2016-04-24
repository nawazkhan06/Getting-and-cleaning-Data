rm(list=ls())
#The training and the test sets are merged to create one (1) data set.
setwd("E:/stdy/assignments/da/UCI HAR Dataset");
#Working directory is set
features     = read.table('./features.txt',header=FALSE); 
#used to import features.txt
activityType = read.table('./activity_labels.txt',header=FALSE);
#used to import activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); 
#used to import subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
#used to import x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); 
#used to import y_train.txt
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
trainingData = cbind(yTrain,subjectTrain,xTrain);
#Used to read in 'test' data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); 
#reads subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); 
#read x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); 
#reads y_test.txt
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";
#Used to assign column names to 'test' data
testData = cbind(yTest,subjectTest,xTest);
#Used to create final test set by merging the data from xTest, yTest and subjectTest 
finalData = rbind(trainingData,testData);
#Used to combine training and test data to create a final data
colNames  = colnames(finalData); 
#Used to create a vector for the column names from the finalData
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
#Used to create a logicalVector that contains TRUE values for ID, mean() & stddev() columns and FALSE for others
finalData = finalData[logicalVector==TRUE];
#Used to Subset finalData table based on the logicalVector
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
#Used to Merge the finalData set with the acitivityType table
colNames  = colnames(finalData); 
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
#Used for updating the colNames vector to include the new column names and clean up vaiable names
colnames(finalData) = colNames;
#Used to Reassign the new descriptive column names to the finalData set
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];
#Used to create a new table, finalDataNoActivityType without the activityType column
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
#Used to Summarize finalDataNoActivityType table to include each variable mean
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
#Used to merging the tidyData with activityType
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');
#Used to automatically Export the tidyData set 
