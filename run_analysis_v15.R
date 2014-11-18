# GCD PROJECT - Samsung Accelerometer Data, Version 15
# Project Data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Create one R script called run_analysis.R that does the following: 
# 1 - Merges the training and the test sets to create one data set.
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 - Uses descriptive activity names to name the activities in the data set
# 4 - Appropriately labels the data set with descriptive variable names. 
# 5 - From the data set in step 4, creates a second, independent tidy data set with 
#     the average of each variable for each activity and each subject.
# Please upload the tidy data set created in step 5 of the instructions. 
# Please upload your data set as a txt file created with write.table() using row.name=FALSE 
#
rm(list=ls())
library(dplyr)
wrkdr <- "C:/Users/Michael Olson/Documents/GitHub/GCD-Project"
setwd(wrkdr); getwd()       # checks for proper working directory for this project
activity_labels <- read.table(file="UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")[,2]  #; activity_labels
feature_nms <- read.table(file="UCI HAR Dataset/features.txt", header = FALSE, sep = "")  # names of features in data sets
subject_train <- read.table(file="UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")
features_train <- read.table(file="UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
activity_train <- read.table(file="UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
subject_test <- read.table(file="UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")
features_test <- read.table(file="UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
activity_test <- read.table(file="UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
dim(subject_train); dim(features_train); dim(activity_train)  # train:  21 subjects, 7352 observations, 5 activities
dim(subject_test); dim(features_test); dim(activity_test)     # test:    9 subjects, 2947 observations, 5 activities
unique(subject_train); dim(unique(subject_train)); unique(subject_test); dim(unique(subject_test))
#
# add names to data frames
names(subject_train) <- "subject"; names(subject_test) = "subject"
# head(subject_train); head(subject_test)
names(activity_train) <- "activity"; names(activity_test) <- "activity"
# head(activity_train); head(activity_test)
names_features <- feature_nms[ ,2]; nms_feat.all <- as.vector(names_features)# gets feature names in a vector
nms_index.mean <- grep("-mean()", nms_feat.all, fixed=TRUE, invert = FALSE)  # mean variable locations
nms_index.std <- grep("-std", nms_feat.all, fixed=TRUE, invert = FALSE)      # std variable locations
nms_index.meanTF <- grepl("-mean()", nms_feat.all)                           # mean variable locations as logical T/F
nms_index.stdTF <- grepl("-std", nms_feat.all)                               # std variable locations as logical T/F
nms_index.TF <- nms_index.meanTF | nms_index.stdTF                           # mean or std locations as logical T/F
# not_nms_index.TF <- ! nms_index.TF                                         # inverted, not (mean or std locations) as logical T/F
#
# simplify names with further abbreviations
nms.feat <- as.vector(feature_nms[nms_index.TF,][,2]); nms.feat              # only mean & std variables as names
nms1 <- gsub("-", "_", nms.feat); nms2 <- gsub("()",  "", nms1, fixed = TRUE); nms3 <- gsub("BodyAcc", "BAc", nms2)
nms4 <- gsub("BAcJerk", "BAcJ", nms3); nms5 <- gsub("GravityAcc",  "GAc", nms4); nms6 <- gsub("BodyGyro", "BGy", nms5)
nms7 <- gsub("fBody",  "fB", nms6); nms8 <- gsub("meanFreq",  "meanF", nms7)
sanms <- c("subject", "activity", nms.feat); sanms    # all variables with original names, with subject & activity
sanmsf <- c("subject", "activity", nms8); sanmsf      # wanted variables only, abreviated, with subject & activity
names(features_train) <- nms_feat.all                 # ; head(features_train)
names(features_test) <- nms_feat.all                  # ; head(features_test)
train.df <- cbind( cbind(subject_train, activity_train), features_train) #; head(train.df)
test.df <- cbind( cbind(subject_test, activity_test), features_test)     #;  head(test.df)
all.df <- rbind(train.df, test.df)       #; dim(all.df); head(all.df)    # data set merged with all variables
#
# convert activity variable to factors with levels   ; class(all.df$activity)
all.df$activity <- factor(all.df$activity, labels = activity_labels )
#
# head(all.df$activity); tail(all.df$activity); class(all.df$activity): head(all.df)   # table structure check
#
# add abbreviated names to desired data subset with only mean() and std() variables
mnstd.df <- all.df[,sanms]; head(mnstd.df)  # data set, only mean & std variables + subject & activity
names(mnstd.df) <- sanmsf; head(mnstd.df)   # data set, only mean & std variables + subject & activity, abreviated names
#
#
ord_mnstd.df <- arrange(mnstd.df, subject, activity)      #; head(ord_mnstd.df[, 1:2], 600)
by_SubAct <- group_by(ord_mnstd.df, subject, activity)    #; head(by_SubAct)[, 1:2]
# colnames(by_SubAct)[3:length(nms8)] <- nms8 ; head(by_SubAct)
#
# long way with summarize since lapply, etc. did not seem to work
test1 <- summarize(by_SubAct, tBAc_mean_X=mean(tBAc_mean_X), tBAc_mean_Y=mean(tBAc_mean_Y), tBAc_mean_Z=mean(tBAc_mean_Z))
test2 <- summarize(by_SubAct, tBAc_std_X=mean(tBAc_std_X), tBAc_std_Y = mean(tBAc_std_Y), tBAc_std_Z = mean(tBAc_std_Z))
test3 <- summarize(by_SubAct, tGAc_mean_X=mean(tGAc_mean_X), tGAc_mean_Y=mean(tGAc_mean_Y),tGAc_mean_Z=mean(tGAc_mean_Z))       
test4 <- summarize(by_SubAct, tGAc_std_X=mean(tGAc_std_X), tGAc_std_Y=mean(tGAc_std_Y), tGAc_std_Z=mean(tGAc_std_Z))
test5 <- summarize(by_SubAct, tBAcJ_mean_X=mean(tBAcJ_mean_X), tBAcJ_mean_Y=mean(tBAcJ_mean_Y), tBAcJ_mean_Z=mean(tBAcJ_mean_Z))    
test6 <- summarize(by_SubAct, tBAcJ_std_X=mean(tBAcJ_std_X), tBAcJ_std_Y=mean(tBAcJ_std_Y), tBAcJ_std_Z=mean(tBAcJ_std_Z))    
test7 <- summarize(by_SubAct, tBGy_mean_X=mean(tBGy_mean_X), tBGy_mean_Y=mean(tBGy_mean_Y), tBGy_mean_Z=mean(tBGy_mean_Z))
test8 <- summarize(by_SubAct, tBGy_std_X=mean(tBGy_std_X), tBGy_std_Y=mean(tBGy_std_Y), tBGy_std_Z=mean(tBGy_std_Z))    
test9 <- summarize(by_SubAct, tBGyJerk_mean_X=mean(tBGyJerk_mean_X), tBGyJerk_mean_Y=mean(tBGyJerk_mean_Y), tBGyJerk_mean_Z=mean(tBGyJerk_mean_Z))  
test10 <- summarize(by_SubAct, tBGyJerk_std_X=mean(tBGyJerk_std_X), tBGyJerk_std_Y=mean(tBGyJerk_std_Y), tBGyJerk_std_Z=mean(tBGyJerk_std_Z))
test11 <- summarize(by_SubAct, tBAcMag_mean=mean(tBAcMag_mean), tBAcMag_std=mean(tBAcMag_std), tGAcMag_mean=mean(tGAcMag_mean))
test12 <- summarize(by_SubAct, tGAcMag_std=mean(tGAcMag_std), tBAcJMag_mean=mean(tBAcJMag_mean), tBAcJMag_std=mean(tBAcJMag_std))    
test13 <- summarize(by_SubAct, tBGyMag_mean=mean(tBGyMag_mean), tBGyMag_std=mean(tBGyMag_std), tBGyJerkMag_mean=mean(tBGyJerkMag_mean))   
test14 <- summarize(by_SubAct, tBGyJerkMag_std=mean(tBGyJerkMag_std), fBAc_mean_X=mean(fBAc_mean_X), fBAc_mean_Y=mean(fBAc_mean_Y), fBAc_mean_Z=mean(fBAc_mean_Z))
test15 <- summarize(by_SubAct, fBAc_std_X=mean(fBAc_std_X), fBAc_std_Y=mean(fBAc_std_Y), fBAc_std_Z=mean(fBAc_std_Z)) 
test16 <- summarize(by_SubAct, fBAmc_meanF_X=mean(fBAc_meanF_X), fBAc_meanF_Y=mean(fBAc_meanF_Y), fBAc_meanF_Y=mean(fBAc_meanF_Y), fBAc_meanF_Z=mean(fBAc_meanF_Z))
test17 <- summarize(by_SubAct, fBAcJ_mean_X=mean(fBAcJ_mean_X), fBAcJ_mean_Y=mean(fBAcJ_mean_Y), fBAcJ_mean_Z=mean(fBAcJ_mean_Z))
test18 <- summarize(by_SubAct, fBAcJ_std_X=mean(fBAcJ_std_X), fBAcJ_std_Y=mean(fBAcJ_std_Y), fBAcJ_std_Z=mean(fBAcJ_std_Z)) 
test19 <- summarize(by_SubAct, fBAcJ_meanF_X=mean(fBAcJ_meanF_X), fBAcJ_meanF_Y=mean(fBAcJ_meanF_Y), fBAcJ_meanF_Z=mean(fBAcJ_meanF_Z))   
test20 <- summarize(by_SubAct, fBGy_mean_X=mean(fBGy_mean_X), fBGy_mean_Y=mean(fBGy_mean_Y), fBGy_mean_Z=mean(fBGy_mean_Z)) 
test21 <- summarize(by_SubAct, fBGy_std_X=mean(fBGy_std_X), fBGy_std_Y=mean(fBGy_std_Y), fBGy_std_Z=mean(fBGy_std_Z))
test22 <- summarize(by_SubAct, fBGy_meanF_X=mean(fBGy_meanF_X), fBGy_meanF_Y=mean(fBGy_meanF_Y), fBGy_meanF_Z=mean(fBGy_meanF_Z))  
test23 <- summarize(by_SubAct, fBAcMag_mean=mean(fBAcMag_mean), fBAcMag_std=mean(fBAcMag_std), fBAcMag_meanF=mean(fBAcMag_meanF))      
test24 <- summarize(by_SubAct, fBBAcJMag_mean=mean(fBBAcJMag_mean), fBBAcJMag_std=mean(fBBAcJMag_std), fBBAcJMag_meanF=mean(fBBAcJMag_meanF))       
test25 <- summarize(by_SubAct, fBBGyMag_mean=mean(fBBGyMag_mean), fBBGyMag_std=mean(fBBGyMag_std), fBBGyMag_meanF=mean(fBBGyMag_meanF))        
test26 <- summarize(by_SubAct, fBBGyJerkMag_mean=mean(fBBGyJerkMag_mean), fBBGyJerkMag_std=mean(fBBGyJerkMag_std), fBBGyJerkMag_meanF=mean(fBBGyJerkMag_meanF))
#
# binary merge tree since merge() seems to prefer two arguments at a time
m1 <- merge(test1, test2); m2 <- merge(test3, test4); m3 <- merge(test5, test6); m4<- merge(test7, test8); m5 <- merge(test9, test10)
m6 <- merge(test11, test12); m7 <- merge(test13, test14); m8 <- merge(test15, test16); m9<- merge(test17, test18); m10 <- merge(test19, test20)
m11 <- merge(test21, test22); m12 <- merge(test23, test24); m13 <- merge(test25, test26)
mm1 <- merge(m1, m2); mm2 <- merge(m3, m4); mm3 <- merge(m5, m6); mm4 <- merge(m7, m8); mm5 <- merge(m9,m10)
mm6 <- merge(m11, m12); mm7 <- merge(m13, m13)
mn1 <- merge(mm1, mm2); mn2 <- merge(mm3, mm4); mn3 <- merge(mm5, mm6); mn4 <- merge(mm7, mm7)
mo1 <- merge(mn1, mn2); mo2 <- merge(mn3, mn4)
mp1 <- merge(mo1, mo2)
#
# Form tidy data set
mq1 <- arrange(mp1, subject)  ##### TIDY DATA SET WITH MEANS of mean() and std() 
#
# checking tidy data set structure
class(mq1); dim(mq1); head(mq1[, 1:2], 180); head(mq1); 
#
# output data set as a txt file created with write.table() using row.name=FALSE 
write.table(mq1, "tidy_data_set.txt", row.name=FALSE)
