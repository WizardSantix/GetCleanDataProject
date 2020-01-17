# CodeBook

This is a codebook that describes the variables, the data, and any transformations or work that were performed to clean up the data.

## Data source

* Original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* Original description of the dataset: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


## Data set information

The experiments were carried out with a group of **30** volunteers within an age bracket of 19-48 years. Each person performed six activities (**WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING**) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Dataset

The original dataset includes these files:

* 'README.txt'

* 'features_info.txt': *Shows information about the variables used on the feature vector.*

* 'features.txt': *List of all features.*

* 'activity_labels.txt': *Links the class labels with their activity name*.

* 'train/X_train.txt': *Training set.*

* 'train/y_train.txt': *Training labels.*

* 'test/X_test.txt': *Test set.*

* 'test/y_test.txt': *Test labels.*

The following files are available for the train and test data. Their descriptions are equivalent. 

* 'train/subject_train.txt': *Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. *

* 'train/Inertial Signals/total_acc_x_train.txt': *The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. *

* 'train/Inertial Signals/body_acc_x_train.txt': *The body acceleration signal obtained by subtracting the gravity from the total acceleration. *

* 'train/Inertial Signals/body_gyro_x_train.txt': *The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. *

## Transformations

The following transformations were made to the dataset, see run_analysis.R for more info. 

* Extracted the measurements on the mean and standard deviation for each measurement.

```{}
# Extract only the measurements on the mean and standard deviation for each measurement.
  feats <- read.table("./UCI HAR Dataset/features.txt")[,2]
  X_test = X_test[,grepl("mean\\(|std", feats)]
  X_train = X_train[,grepl("mean\\(|std", feats)]
```

* The activity identifiers (originally coded as integers between 1 and 6) were replaced with descriptive activity names.
```{}
# Translate the numbers in y_test for their activity names
  activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
  y_test = activity_labels[y_test[,1]]
  y_train = activity_labels[y_train[,1]]
```

* Merged the training and the test sets to create one data set that was melted to make it tidy.
```{}
# Bind data
  test_data <- cbind(subject_test, Activity = y_test, X_test)
  train_data <- cbind(subject_train, Activity= y_train, X_train)
  data <- rbind(test_data, train_data)
  id   = c("DataGroup","Subject", "Activity")
  labels = setdiff(colnames(data), id)
  melted_data   = melt(data, id = id, measure.vars = labels, variable.name = "Variable", value.name = "Value")
```  

* Created a second, independent tidy data set with the average of each variable for each activity and each subject.
```{}
# Average of each variable for each activity and each subject
  meanvar<- dcast(melted_data, Subject + Activity ~ Variable, mean)
```

## Variables

### Subject
The test subject of the experiment identified with a number ranging from 1 to 30

### Activity
One of the 6 possible activities that were performed in the experiment (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

### Average of Measurements: 
All 66 measurements are floating-point values, normalised and bounded within [-1,1].

Prior to normalisation, acceleration measurements (variables containing Accelerometer) were made in g's (9.81 m.s⁻²) and gyroscope measurements (variables containing Gyroscope) were made in radians per second (rad.s⁻¹).

Magnitudes of three-dimensional signals (variables containing Magnitude) were calculated using the Euclidean norm.

The measurements are classified in two domains:

Time-domain signals (variables prefixed by a **t**), resulting from the capture of accelerometer and gyroscope raw signals.

Frequency-domain signals (variables prefixed by a **f**), resulting from the application of a Fast Fourier Transform (FFT) to some of the time-domain signals.

#### Time-domain signals

- Average time-domain body acceleration in the X, Y and Z directions:

	- `tBodyAcc-mean()-X`
	- `tBodyAcc-mean()-Y`
	- `tBodyAcc-mean()-Z`

- Standard deviation of the time-domain body acceleration in the X, Y and Z directions:

	- `tBodyAcc-std()-X`
	- `tBodyAcc-std()-Y`
	- `tBodyAcc-std()-Z`

- Average time-domain gravity acceleration in the X, Y and Z directions:

	- `tGravityAcc-mean()-X`
	- `tGravityAcc-mean()-Y`
	- `tGravityAcc-mean()-Z`

- Standard deviation of the time-domain gravity acceleration in the X, Y and Z directions:

	- `tGravityAcc-std()-X`
	- `tGravityAcc-std()-Y`
	- `tGravityAcc-std()-Z`

- Average time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `tBodyAccJerk-mean()-X`
	- `tBodyAccJerk-mean()-Y`
	- `tBodyAccJerk-mean()-Z`

- Standard deviation of the time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `tBodyAccJerk-std()-X`
	- `tBodyAccJerk-std()-Y`
	- `tBodyAccJerk-std()-Z`

- Average time-domain body angular velocity in the X, Y and Z directions:

	- `tBodyGyro-mean()-X`
	- `tBodyGyro-mean()-Y`
	- `tBodyGyro-mean()-Z`

- Standard deviation of the time-domain body angular velocity in the X, Y and Z directions:

	- `tBodyGyro-std()-X`
	- `tBodyGyro-std()-Y`
	- `tBodyGyro-std()-Z`

- Average time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions:

	- `tBodyGyroJerk-mean()-X`
	- `tBodyGyroJerk-mean()-Y`
	- `tBodyGyroJerk-mean()-Z`

- Standard deviation of the time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions:

	- `tBodyGyroJerk-std()-X`
	- `tBodyGyroJerk-std()-Y`
	- `tBodyGyroJerk-std()-Z`

- Average and standard deviation of the time-domain magnitude of body acceleration:

	- `tBodyAccMag-mean()`
	- `tBodyAccMag-std()`

- Average and standard deviation of the time-domain magnitude of gravity acceleration:

	- `tGravityAccMag-mean()`
	- `tGravityAccMag-std()`

- Average and standard deviation of the time-domain magnitude of body acceleration jerk (derivation of the acceleration in time):

	- `tBodyAccJerkMag-mean()`
	- `tBodyAccJerkMag-std()`

- Average and standard deviation of the time-domain magnitude of body angular velocity:

	- `tBodyGyroMag-mean()`
	- `tBodyGyroMag-std()`

- Average and standard deviation of the time-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time):

	- `tBodyGyroJerkMag-mean()`
	- `tBodyGyroJerkMag-std()`

#### Frequency-domain signals

- Average frequency-domain body acceleration in the X, Y and Z directions:

	- `fBodyAcc-mean()-X`
	- `fBodyAcc-mean()-y`
	- `fBodyAcc-mean()-Z`

- Standard deviation of the frequency-domain body acceleration in the X, Y and Z directions:

	- `fBodyAcc-std()-X`
	- `fBodyAcc-std()-Y`
	- `fBodyAcc-std()-Z`

- Average frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `fBodyAccJerk-mean()-X`
	- `fBodyAccJerk-mean()-Y`
	- `fBodyAccJerk-mean()-Z`

- Standard deviation of the frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `fBodyAccJerk-std()-X`
	- `fBodyAccJerk-std()-Y`
	- `fBodyAccJerk-std()-Z`

- Average frequency-domain body angular velocity in the X, Y and Z directions:

	- `fBodyGyro-mean()-X`
	- `fBodyGyro-mean()-Y`
	- `fBodyGyro-mean()-Z`

- Standard deviation of the frequency-domain body angular velocity in the X, Y and Z directions:

	- `fBodyGyro-std()-X`
	- `fBodyGyro-std()-Y`
	- `fBodyGyro-std()-Z`

- Average, standard deviation of the frequency components of the frequency-domain magnitude of body acceleration:

	- `fBodyAccMag-mean()`
	- `fBodyAccMag-std()`

- Average, standard deviation of the frequency components of the frequency-domain magnitude of body acceleration jerk (derivation of the acceleration in time):

	- `fBodyBodyAccJerkMag-mean()`
	- `fBodyBodyAccJerkMag-std()`

- Average, standard deviation of the frequency components of the frequency-domain magnitude of body angular velocity:

	- `fBodyBodyGyroMag-mean()`
	- `fBodyBodyGyroMag-std()`

- Average, standard deviation of the frequency components of the frequency-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time):

	- `fBodyBodyGyroJerkMag-mean()`
	- `fBodyBodyGyroJerkMag-std()`.
