setwd("P:/Coursera/datacleaning/courseproject/UCI HAR Dataset/")
library(dplyr)

train_files<-paste0("./train/",list.files("./train/",pattern=".txt", recursive=FALSE)) #gets vector of the files to load
test_files<-paste0("./test/",list.files("./test/",pattern=".txt", recursive=FALSE)) 
##all_files<-c(train_files,test_files)

feature_names<-read.table("features.txt") #gets feature names
activity_labels = read.table('activity_labels.txt') #gets actvity labels

train_data<-lapply(train_files,read.table) #uses lapply to load  data to list
test_data<-lapply(test_files,read.table)

##naming variables and lists
names(train_data)<-gsub(".*/|.txt.*","",train_files) #names the list objects - optional
names(test_data)<-gsub(".*/|.txt.*","",test_files)

names(train_data[[1]])<-c("subject") #names the variables in each list
names(train_data[[2]])<-feature_names$V2
names(train_data[[3]])<-c("activity")

names(test_data[[1]])<-c("subject")
names(test_data[[2]])<-feature_names$V2
names(test_data[[3]])<-c("activity")


##binds the list elements to complete the dataset
train_complete<-cbind(train_data[[1]],train_data[[3]],train_data[[2]])
test_complete<-cbind(test_data[[1]],test_data[[3]],test_data[[2]])
combined_dataset<-rbind(train_complete,test_complete)

##gets the vars related to mean and std
vars_to_get_logical<- (grepl("subject",names(combined_dataset)) | grepl("activity",names(combined_dataset)) | grepl("-mean",names(combined_dataset)) | grepl("-std",names(combined_dataset))) & !grepl("-meanFreq",names(combined_dataset))

##gets only the data we require
complete_dataset<-combined_dataset[vars_to_get_logical]


# Clean up variable names
for (i in 1:length(colnames(complete_dataset))) 
{
  colnames(complete_dataset)[i] = gsub("\\()","",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("-std","StdDev",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("-mean","Mean",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("^(t)","time-",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("^(f)","freq-",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("AccMag","AccMagnitude",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("JerkMag","JerkMagnitude",colnames(complete_dataset)[i])
  colnames(complete_dataset)[i] = gsub("GyroMag","GyroMagnitude",colnames(complete_dataset)[i])
};

#create new tidy dataset with means of vars grouped by subject and activity
tidy_data<-as.data.frame(aggregate(complete_dataset[,!names(complete_dataset) %in% c("activity","subject")],by=list(complete_dataset$subject,complete_dataset$activity),FUN=mean))

#rename group-by columns to reflect what they are
colnames(tidy_data)[1:2]<-c("subject","activity")

#replace activity with descriptive

tidy_data$activity<-activity_labels$V2[tidy_data$activity]

# Export the tidyData set 
write.table(tidy_data, './tidy_data.txt',row.names=TRUE,sep='\t');
