library(dplyr)
library(plyr)
##1Merges the training and the test sets to create one data set & 3 Uses descriptive activity names to name the activities in the data set (see line 9 and 29)
#Read training data 
data_X_train<- read.table("X_train.txt",header=FALSE)
#add features identifiers
features_train<-read.table("features.txt",header=FALSE)
names<-features_train$V2
names(data_X_train)<-names
#add subject id and type id
data_X_train$type<-c("training") 
data_X_train$key<-1:nrow(data_X_train)
#add training labels 
data_Y_train<- read.table("Y_train.txt",header=FALSE)
names(data_Y_train)<-c("labels")
data_Y_train$labels<-factor(data_Y_train$labels,levels=c(1,2,3,4,5,6),labels=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
data_Y_train$key<-1:nrow(data_Y_train)
#add training subject id 
data_subject_train<- read.table("subject_train.txt",header=FALSE)
names(data_subject_train)<-c("subject")
data_subject_train$key<-1:nrow(data_subject_train)
#Merge training set
aux_merged_train<-merge(data_X_train,data_Y_train, by="key")
merged_train<-merge(aux_merged_train,data_subject_train, by="key")
#Read testing data 
data_X_test<- read.table("X_test.txt",header=FALSE)
#**
#add features identifiers
#**
#cambiar nombre a _test para no pisar el anterior, poner al anterior features_train
features_test<-read.table("features.txt",header=FALSE)
#tiene los nom
#**
names<-features_test$V2
names(data_X_test)<-names
#add subject id and type id
data_X_test$type<-c("testing") 
data_X_test$key<-1:nrow(data_X_test)
#add testing labels 
data_Y_test<- read.table("Y_test.txt",header=FALSE)
names(data_Y_test)<-c("labels")
data_Y_test$labels<-factor(data_Y_test$labels,levels=c(1,2,3,4,5,6),labels=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
data_Y_test$key<-1:nrow(data_Y_test)
#add testing subject id 
data_subject_test<- read.table("subject_test.txt",header=FALSE)
names(data_subject_test)<-c("subject")
data_subject_test$key<-1:nrow(data_subject_test)
#Merge testing set
aux_merged_test<-merge(data_X_test,data_Y_test, by="key")
merged_test<-merge(aux_merged_test,data_subject_test, by="key")
#check dimensions, columm number must match and they must be in the same order in both data sets
dim(merged_train)
dim(merged_test)
#Append training & testing set
append_data<-rbind(merged_train,merged_test)

#2.Extract mean & std for each measurement 
#The index is plus one because id is the first variable
aux_measurement_index_mean<-grep("mean",names)
aux_measurement_index_std<-as.vector(grep("std",names))
measurement_index<-c(aux_measurement_index_mean,aux_measurement_index_std)
#col_key<-match("key", names(append_data))
col_type<-match("type", names(append_data))
col_labels<-match("labels", names(append_data))
col_subject<-match("subject", names(append_data))
index_extract<-c(measurement_index,col_type,col_labels,col_subject)
append_data_clean_aux<-append_data[,index_extract]
#remove the meanFreq variables
aux_names<-names(append_data_clean_aux)
aux_measurement_index_freq<-grep("Freq",aux_names)
#There are 6 columns with frequency measures
append_data_clean<-append_data_clean_aux[,-aux_measurement_index_freq]


#4.Appropriately labels the data set with descriptive variable names.
library(Hmisc)
comment(append_data_clean)<-"The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 
3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and
gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag,
tBodyGyroJerkMag). Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag,
fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:  '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
Note:Features are normalized and bounded within [-1,1]."

# 5. create a second data set with the average of each variable for each activity and each subject.
summary_data<-aggregate(append_data_clean[,2:73], list(append_data_clean$subject,append_data_clean$labels), mean )
#check dimensions, there are 180 rows (30 subjects with 6 activities each) and 74 columns with the average of each measure for each subject and activity
dim(summary_data)


#create codebook
library(memisc)
codebook<-codebook(summary_data)
Write(codebook(summary_data),file="codebook.txt")

#create final output
result<-write.table(summary_data,file="result.txt",row.name=FALSE) 