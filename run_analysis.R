####load datas and package
library(dplyr)
train_value<-read.table(file="UCI HAR Dataset/train/X_train.txt",header=FALSE)
test_value<-read.table(file="UCI HAR Dataset/test/X_test.txt",header=FALSE)

train_label<-read.table(file="UCI HAR Dataset/train/Y_train.txt",header=FALSE)
test_label<-read.table(file="UCI HAR Dataset/test/Y_test.txt",header=FALSE)

train_sub<-read.table(file="UCI HAR Dataset/train/subject_train.txt",header=FALSE)
test_sub<-read.table(file="UCI HAR Dataset/test/subject_test.txt",header=FALSE)

names<-read.table(file="UCI HAR Dataset/features.txt",header=FALSE)
activity<-read.table(file="UCI HAR Dataset/activity_labels.txt",header=FALSE)

###change names of each data
names(train_sub)<-"subject"
names(test_sub)<-"subject"
names(train_label)<-"activity_no"
names(test_label)<-"activity_no"
names(train_value)<-names$V2
names(test_value)<-names$V2
names(activity)<-c("activity_no","activity")

####Uses descriptive activity names to name the activities in the data set
train_label<-merge(train_label,activity)
test_label<-merge(test_label,activity)

###Merges the training and the test sets to create one data set
train<-data.frame(type=1,train_sub,train_label,train_value)
test<-data.frame(type=2,test_sub,test_label,test_value)
total<-bind_rows(train,test)

###Extracts only the measurements on the mean and standard deviation for each measurement
names_t<-names(total)
con1<-grepl("mean",names_t)
con2<-grepl("std",names_t)
con3<-grepl("meanFreq",names_t)
con<-(con1 & !con3) | con2
subset<-data.frame(select(total,c(1,2,4)),select(total,which(con)))


###creates a second, independent tidy data set with the average of each variable 
###for each activity and each subject
final<-data.frame(subject=rep(1:30,6),activity=rep(c("LAYING","SITTING","STANDING","WALKING",
                                                     "WALKING_DOWNSTAIRS","WALKING_UPSTAIRS"),each=30))
for (i in 4:69){
  mean<-tapply(subset[,i],list(subset$subject,subset$activity),mean)
  raw<-data.frame(subject=rep(1:30,6),activity=rep(c("LAYING","SITTING","STANDING","WALKING",
                                                     "WALKING_DOWNSTAIRS","WALKING_UPSTAIRS"),
                                                   each=30),mean=mean[1:180])
  raw<-filter(raw,!is.na(raw$mean))
  names(raw)[3]<-names(subset)[i]
  final<-merge(final,raw)
}

####write txt file
final<-arrange(final,subject)
write.table(final,file="final.txt",row.name=FALSE) 

