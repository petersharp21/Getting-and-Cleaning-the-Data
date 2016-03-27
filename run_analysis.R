## Set the Working Directory
PATH <- "C:/Users/Peter/Documents/1_DATA_SCIENCE/Getting and Cleaning Data/Project"
setwd(PATH)

## Create folder "download_data" inside Path
dir.create("./downloaded_data")

## Download the dataset
DATAURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "./downloaded_data/UCI_HAR_Data.zip"
download.file(DATAURL,destfile)

## Decompress file in Working Directory
unzip(zipfile="./downloaded_data/UCI_HAR_Data.zip",exdir="./downloaded_data/files")

## Load Packages: dplyr; data.table; tidyr
require(dplyr)
require(data.table)
require(tidyr)

## Read files from UCI HAR Dataset
files_path <- "C:/Users/Peter/Documents/1_DATA_SCIENCE/Getting and Cleaning Data/Project/downloaded_data/files/UCI HAR Dataset"

## Read subject Train files
Sub_Train <- tbl_df(read.table(file.path(files_path, "train", "subject_train.txt")))
Act_Train <- tbl_df(read.table(file.path(files_path, "train", "Y_train.txt")))
Data_Train <- tbl_df(read.table(file.path(files_path, "train", "X_train.txt" )))

## Read subject Test files
Sub_Test <- tbl_df(read.table(file.path(files_path, "test" , "subject_test.txt" )))
Act_Test <- tbl_df(read.table(file.path(files_path, "test" , "Y_test.txt" )))
Data_Test  <- tbl_df(read.table(file.path(files_path, "test" , "X_test.txt" )))

## Read Acctivity labels and Features labels and set proper column names
Act_Labels <- tbl_df(read.table(file.path(files_path, "activity_labels.txt" )))
setnames(Act_Labels, names(Act_Labels), c("Act_Number","Act_Name"))

Feat_Labels <- tbl_df(read.table(file.path(files_path, "features.txt" )))
setnames(Feat_Labels, names(Feat_Labels), c("Feat_Number","Feat_Name"))


## Merge "Sub_Test" and "Sub_Train" files into one and rename "V1" to "Subject"
Sub_All <- rbind(Sub_Train, Sub_Test)
setnames(Sub_All, "V1", "subject")

## Merge "Act_Test" and "Act_Train" files into one and rename "V1" to "Act_Number"
Act_All <- rbind(Act_Train, Act_Test)
setnames(Act_All, "V1", "Act_Number")

## Merge the data of both "Data_Train" and "Data_Test" and rename variables based on "Feat_Labels"
Combined_Data <- rbind(Data_Train, Data_Test)
colnames(Combined_Data) <- Feat_Labels$Feat_Name

## Combine all columns from "Sub_All" and "Act_All" and into "Combined_Data"
All_Data <- cbind(Sub_All,Act_All)
Combined_Data <- cbind(All_Data, Combined_Data)

## Extracts only the measurements on the mean and standard deviation for each measurement from the "Feat_Labels" and
## subsets "Combined_Data" based on those measurements only
Feat_Wanted <- grep("mean\\(\\)|std\\(\\)",Feat_Labels$Feat_Name,value=TRUE)
Feat_Wanted <- union(c("subject","Act_Number"), Feat_Wanted)
Combined_Data <- subset(Combined_Data,select=Feat_Wanted)

## Uses descriptive activity names to name the activities in the data set
Combined_Data <- merge(Act_Labels, Combined_Data , by="Act_Number", all.x=TRUE)
Combined_Data$Act_Name <- as.character(Combined_Data$Act_Name)

## Create Combined_Data with variable means sorted by subject and Act_Name
Combined_Data$Act_Name <- as.character(Combined_Data$Act_Name)
Aggr_Data <- aggregate(. ~ subject - Act_Name, data = Combined_Data, mean)
Combined_Data <- tbl_df(arrange(Aggr_Data,subject,Act_Name))

## Appropriately labels the data set with descriptive variable names
names(Combined_Data)<-gsub("std()", "SD", names(Combined_Data))
names(Combined_Data)<-gsub("mean()", "MEAN", names(Combined_Data))
names(Combined_Data)<-gsub("^t", "time", names(Combined_Data))
names(Combined_Data)<-gsub("^f", "frequency", names(Combined_Data))
names(Combined_Data)<-gsub("Acc", "Accelerometer", names(Combined_Data))
names(Combined_Data)<-gsub("Gyro", "Gyroscope", names(Combined_Data))
names(Combined_Data)<-gsub("Mag", "Magnitude", names(Combined_Data))
names(Combined_Data)<-gsub("BodyBody", "Body", names(Combined_Data))

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject
write.table(Combined_Data, "Tidy_Dataset.txt", row.name=FALSE)
