# This R script does the following:
# 1. Merges the training and the test sets to create one data set.
train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
set_dt_X <- rbind(train, test)
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
set_dt_S <- rbind(subject_train, subject_test)
y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
set_dt_Y <- rbind(y_train, y_test)
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
feat <- read.table("features.txt")
ind_features <- grep("-mean\\(\\)|-std\\(\\)", feat[, 2])
set_dt_X  <- set_dt_X [, ind_features]
names(set_dt_X) <- feat[ind_features, 2]
names(set_dt_X) <- gsub("\\(|\\)", "", names(set_dt_X))
names(set_dt_X) <- tolower(names(set_dt_X))
# 3. Uses descriptive activity names to name the activities in the data set.
act <- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", tolower(as.character(act[, 2])))
set_dt_Y[,1] = act[set_dt_Y[,1], 2]
names(set_dt_Y) <- "activity"
# 4. Appropriately labels the data set with descriptive activity names.
names(set_dt_S) <- "subject"
t_data <- cbind(set_dt_S, set_dt_Y, set_dt_X)
write.table(t_data, "clean_data.txt")
# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubj = unique(set_dt_S)[,1]
numSubj = length(unique(set_dt_S)[,1])
numAct = length(act[,1])
numCols = dim(t_data)[2]
res = t_data[1:(numSubj*numAct), ]
row = 1
for (nsubj in 1:numSubj) {
  for (nact in 1:numAct) {
    res[row, 1] = uniqueSubj[nsubj]
    res[row, 2] = act[nact, 2]
    tmp <- t_data[t_data$subject==nsubj & t_data$activity==act[nact, 2], ]
    res[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(res, "dt_st_w_aver.txt",row.name=FALSE)