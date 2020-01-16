## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("tidyr")) {
  install.packages("tidyr")
}

# Load: data column names and activity labels
feats <- read.table("./UCI HAR Dataset/features.txt")[,2]
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

#Process test data
  
  # Load and process X_test, y_test and test_subject data.
  X_test <- (read.table("./UCI HAR Dataset/test/X_test.txt"))
  y_test <- (read.table("./UCI HAR Dataset/test/y_test.txt"))
  subject_test <-(read.table("./UCI HAR Dataset/test/subject_test.txt"))

  #Name the variables in x_test
  names(X_test) = feats
  names(subject_test)="Subject"

  # Extract only the measurements on the mean and standard deviation for each measurement.
  X_test = X_test[,grepl("mean\\(|std", feats)]

  # Translate the numbers in y_test for their activity names
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
  y_test = activity_labels[y_test[,1]]
  
  # Bind data
  test_data <- cbind(subject_test, Activity = y_test, X_test)

  
#Process Train data

  # Load and process X_test, y_test and test_subject data.
  X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

  #Name the variables in x_test
  names(X_train) = feats
  names(subject_train)="Subject"
  names(y_train)="Activity"

  # Extract only the measurements on the mean and standard deviation for each measurement.
  X_train = X_train[,grepl("mean\\(|std", feats)]

  # Translate the numbers in y_test for their activity names
  y_train = activity_labels[y_train[,1]]

  # Bind data
  train_data <- cbind(subject_train, Activity= y_train, X_train)
  
  
# Merge test and train data
  #Set test/train group id for each dataset
  test_data<- test_data %>% mutate(DataGroup= "Test") %>% select(DataGroup,everything())
  train_data<- train_data %>% mutate(DataGroup= "Train") %>% select(DataGroup,everything()) 
  
  # Merge test and train data
  data <- rbind(test_data, train_data)
  
  id   = c("DataGroup","Subject", "Activity")
  labels = setdiff(colnames(data), id)
  melted_data   = melt(data, id = id, measure.vars = labels, variable.name = "Variable", value.name = "Value")
  
# Average of each variable for each activity and each subject
  meanvar<- dcast(melted_data, Subject + Activity ~ Variable, mean)

#Write .txt with the dataset
  write.table(meanvar, file = "./tidy_data.txt")