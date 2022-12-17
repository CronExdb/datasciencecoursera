library(tidyverse)

setwd("C:/Users/CH/Rproj/ProjCursera/datasciencecoursera/New folder/UCI HAR Dataset")






features <- read.table("features.txt", col.names = c("nr","features"))
activity_labels <- read.table("activity_labels.txt", col.names = c("code", "activity")) 
subject_test <- read.table("./test/subject_test.txt", col.names = "subject")
x_test <- read.table("./test/X_test.txt", col.names = features$features)

y_test <- read.table("./test/y_test.txt", col.names = "code")
subject_train <- read.table("./train/subject_train.txt", col.names = "subject")
x_train <- read.table("./train/X_train.txt", col.names = features$features)
y_train <- read.table("./train/y_train.txt", col.names = "code")


Xdata <- rbind(x_train, x_test)
Ydata <- rbind(y_train, y_test)
Subject_data <- rbind(subject_train, subject_test)

Merged_data <- cbind(Subject_data,Ydata,Xdata)




select_measurements<- Merged_data %>% select(subject, code, contains("mean"), contains("std"))

select_measurements$code <- activity_labels[select_measurements$code, 2]  


select_measurements <- rename(select_measurements, "activity" = 2) 
names(select_measurements)<-gsub("Acc", "Accelerometer", names(select_measurements))
names(select_measurements)<-gsub("Gyro", "Gyroscope", names(select_measurements))
names(select_measurements)<-gsub("BodyBody", "Body", names(select_measurements))
names(select_measurements)<-gsub("Mag", "Magnitude", names(select_measurements))
names(select_measurements)<-gsub("^t", "Time", names(select_measurements))
names(select_measurements)<-gsub("^f", "Frequency", names(select_measurements))
names(select_measurements)<-gsub("tBody", "TimeBody", names(select_measurements))
names(select_measurements)<-gsub("angle", "Angle", names(select_measurements))
names(select_measurements)<-gsub("gravity", "Gravity", names(select_measurements))




Data <- select_measurements %>% group_by(subject, activity) %>%  summarise_all(mean)
write.table(Data, "tidy_data_set.txt", row.name=FALSE)






