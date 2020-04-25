################################################
# run_analysis.R
# Version 1.0
#
# By Christine Yip
################################################



###################################
#READ IN THE RELEVANT PACKAGES
###################################

install.packages("expss")
library(expss)

install.packages("dplyr")
library(dplyr)

install.packages("plyr")
library(plyr)



#############################
# READ IN THE DATASETS
#############################

#read in the labels and features
activity_labels<-read.table(file="activity_labels.txt", header = FALSE, sep = "")
features<-read.table(file="features.txt", header = FALSE, sep = "")


#read in the test data
subject_test<-read.table(file="subject_test.txt", header = FALSE, sep = "")
x_test<-read.table(file="X_test.txt", header = FALSE, sep = "")
y_test<-read.table(file="y_test.txt", header = FALSE, sep = "")

#read in the train data
subject_train<-read.table(file="subject_train.txt", header = FALSE, sep = "")
x_train<-read.table(file="X_train.txt", header = FALSE, sep = "")
y_train<-read.table(file="y_train.txt", header = FALSE, sep = "")



####################################################################################
#RENAME THE VARIABLES IN THE X_TEST AND X_TRAIN TO THEIR RELEVANT FEATURE NAMES
####################################################################################

for (i in 1:ncol(x_test)) {
  for (j in 1:nrow(features)){
    if (i == features[j,1]) {
      names(x_test)[i]<-as.character(features[j,2])
    }
  }
}

for (i in 1:ncol(x_train)) {
  for (j in 1:nrow(features)){
    if (i == features[j,1]) {
      names(x_train)[i]<-as.character(features[j,2])
    }
  }
}



#######################################################################################
# IDENTIFY WHICH FEATURES ARE THE MEAN AND STANDARD DEVIATIONS OF THE MEASUREMENTS
#######################################################################################

#identify the columns that have mean and std
features_mean<-grep("-mean\\(\\)", features$V2, value=FALSE)
features_std<-grep("-std\\(\\)", features$V2, value=FALSE)

#combine and sort into one vector
features_mean_std<-c(features_mean, features_std)
features_mean_std<-sort(features_mean_std)



#####################################################################################################################
# SELECT ONLY THE VARIABLES IN THE X_TEST AND X_TRAIN THAT ARE THE MEAN AND STANDARD DEVIATIONS OF THE MEASURES
#####################################################################################################################

select_x_test<-x_test[,features_mean_std]
select_x_train<-x_train[,features_mean_std]



#########################################################################################
# UPDATE THE Y_TEST AND Y_TRAIN TO INCLUDE A VARIABLE WITH THE ACTUAL ACTIVITY NAMES
#########################################################################################

act_y_test<-y_test
act_y_train<-y_train

for (i in 1:nrow(act_y_test)){
  for (j in 1:nrow(activity_labels)){
    if (act_y_test[i,1] == activity_labels[j,1]) {
      act_y_test[i,2] <- activity_labels[j,2]
    }
  }
}

for (i in 1:nrow(act_y_train)){
  for (j in 1:nrow(activity_labels)){
    if (act_y_train[i,1] == activity_labels[j,1]) {
      act_y_train[i,2] <- activity_labels[j,2]
    }
  }
}

names(act_y_test)[1]<-"Activity_Number"
names(act_y_test)[2]<-"Activity_Character"

names(act_y_train)[1]<-"Activity_Number"
names(act_y_train)[2]<-"Activity_Character"



##########################################################################
# RENAME THE SUBJECT VARIABLE IN THE SUBJECT_TEST AND SUBJECT_TRAIN
##########################################################################

names(subject_test)[1]<-"Subject_ID"
names(subject_train)[1]<-"Subject_ID"



###########################################################################
# COMBINE AND STACK TOGETHER THE SUBJECT, X, AND Y DATASETS INTO ONE
###########################################################################

combined_test<-cbind(subject_test,select_x_test, act_y_test)
combined_train<-cbind(subject_train,select_x_train, act_y_train)

combined_test_train<-rbind(combined_test,combined_train)



#########################################################
# ADD DESCRIPTIVE LABELS TO THE COMBINED DATASET
#########################################################

combined_test_train_lbl<-apply_labels(combined_test_train,
                                      Subject_ID = "The subject identification number",
                                      Activity_Number = "The activity the subject engaged in (as a categorical number)",
                                      Activity_Character = "The activity the subject engaged in",
                                      "tBodyAcc-mean()-X" = "The mean of the time domain of the body acceleration signal along the X-axis",
                                      "tBodyAcc-mean()-Y" = "The mean of the time domain of the body acceleration signal along the Y-axis",           
                                      "tBodyAcc-mean()-Z" = "The mean of the time domain of the body acceleration signal along the Z-axis",          
                                      "tBodyAcc-std()-X" = "The standard deviation of the time domain of the body acceleration signal along the X-axis",             
                                      "tBodyAcc-std()-Y" = "The standard deviation of the time domain of the body acceleration signal along the Y-axis",           
                                      "tBodyAcc-std()-Z" = "The standard deviation of the time domain of the body acceleration signal along the Z-axis",            
                                      "tGravityAcc-mean()-X" = "The mean of the time domain of the gravity acceleration signal along the X-axis",     
                                      "tGravityAcc-mean()-Y" = "The mean of the time domain of the gravity acceleration signal along the Y-axis",           
                                      "tGravityAcc-mean()-Z" = "The mean of the time domain of the gravity acceleration signal along the Z-axis",           
                                      "tGravityAcc-std()-X" = "The standard deviation of the time domain of the gravity acceleration signal along the X-axis",           
                                      "tGravityAcc-std()-Y" = "The standard deviation of the time domain of the gravity acceleration signal along the Y-axis",       
                                      "tGravityAcc-std()-Z" = "The standard deviation of the time domain of the gravity acceleration signal along the Z-axis",        
                                      "tBodyAccJerk-mean()-X" = "The mean of the time domain of the jerk signals of the body linear acceleration along the X-axis",      
                                      "tBodyAccJerk-mean()-Y" = "The mean of the time domain of the jerk signals of the body linear acceleration along the Y-axis",        
                                      "tBodyAccJerk-mean()-Z" = "The mean of the time domain of the jerk signals of the body linear acceleration along the Z-axis",       
                                      "tBodyAccJerk-std()-X" = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the X-axis",         
                                      "tBodyAccJerk-std()-Y" = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the Y-axis",         
                                      "tBodyAccJerk-std()-Z" = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the Z-axis",         
                                      "tBodyGyro-mean()-X" = "The mean of the time domain of the body gyroscope signal along the X-axis",           
                                      "tBodyGyro-mean()-Y" = "The mean of the time domain of the body gyroscope signal along the Y-axis",         
                                      "tBodyGyro-mean()-Z" = "The mean of the time domain of the body gyroscope signal along the Z-axis",          
                                      "tBodyGyro-std()-X" = "The standard deviation of the time domain of the body gyroscope signal along the X-axis",          
                                      "tBodyGyro-std()-Y" = "The standard deviation of the time domain of the body gyroscope signal along the Y-axis",          
                                      "tBodyGyro-std()-Z" = "The standard deviation of the time domain of the body gyroscope signal along the Z-axis",           
                                      "tBodyGyroJerk-mean()-X" = "The mean of the time domain of the jerk signals of the body gyroscope signal along the X-axis",      
                                      "tBodyGyroJerk-mean()-Y" = "The mean of the time domain of the jerk signals of the body gyroscope signal along the Y-axis",        
                                      "tBodyGyroJerk-mean()-Z" = "The mean of the time domain of the jerk signals of the body gyroscope signal along the Z-axis",        
                                      "tBodyGyroJerk-std()-X" = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the X-axis",         
                                      "tBodyGyroJerk-std()-Y" = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the Y-axis",      
                                      "tBodyGyroJerk-std()-Z" = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the Z-axis",      
                                      "tBodyAccMag-mean()" = "The mean of the time domain of the magnitude of the body linear acceleration",        
                                      "tBodyAccMag-std()" = "The standard deviation of the time domain of the magnitude of the body linear acceleration",           
                                      "tGravityAccMag-mean()" = "The mean of the time domain of the magnitude of the gravity acceleration signal",      
                                      "tGravityAccMag-std()"  = "The standard deviation of the time domain of the magnitude of the gravity acceleration signal",       
                                      "tBodyAccJerkMag-mean()" = "The mean of the time domain of the magnitude of the jerk signals of the body linear acceleration",    
                                      "tBodyAccJerkMag-std()" = "The standard deviation of the time domain of the magnitude of the jerk signals of the body linear acceleration",       
                                      "tBodyGyroMag-mean()" = "The mean of the time domain of the magnitude of the body gyroscope signal",         
                                      "tBodyGyroMag-std()" = "The standard deviation of the time domain of the magnitude of the body gyroscope signal",           
                                      "tBodyGyroJerkMag-mean()" = "The mean of the time domain of the magnitude of the jerk signals of the body gyroscope signal",    
                                      "tBodyGyroJerkMag-std()" = "The standard deviation of the time domain of the magnitude of the jerk signals of the body gyroscope signal",        
                                      "fBodyAcc-mean()-X" = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the X-axis",          
                                      "fBodyAcc-mean()-Y" = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the Y-axis",            
                                      "fBodyAcc-mean()-Z" = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the Z-axis",           
                                      "fBodyAcc-std()-X" = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the X-axis",              
                                      "fBodyAcc-std()-Y" = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the Y-axis",              
                                      "fBodyAcc-std()-Z" = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the Z-axis",              
                                      "fBodyAccJerk-mean()-X" = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the X-axis",      
                                      "fBodyAccJerk-mean()-Y" = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Y-axis",      
                                      "fBodyAccJerk-mean()-Z" = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Z-axis",      
                                      "fBodyAccJerk-std()-X" = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the X-axis",       
                                      "fBodyAccJerk-std()-Y" = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Y-axis",       
                                      "fBodyAccJerk-std()-Z" = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Z-axis",        
                                      "fBodyGyro-mean()-X" = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the X-axis",         
                                      "fBodyGyro-mean()-Y" = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Y-axis",           
                                      "fBodyGyro-mean()-Z" = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Z-axis",           
                                      "fBodyGyro-std()-X" = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the X-axis",            
                                      "fBodyGyro-std()-Y" = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Y-axis",          
                                      "fBodyGyro-std()-Z" = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Z-axis",          
                                      "fBodyAccMag-mean()" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the body linear acceleration",        
                                      "fBodyAccMag-std()" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the body linear acceleration",           
                                      "fBodyBodyAccJerkMag-mean()" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body linear acceleration",   
                                      "fBodyBodyAccJerkMag-std()" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body linear acceleration",  
                                      "fBodyBodyGyroMag-mean()" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the body gyroscope signal",   
                                      "fBodyBodyGyroMag-std()" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the body gyroscope signal",         
                                      "fBodyBodyGyroJerkMag-mean()" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body gyroscope signal",   
                                      "fBodyBodyGyroJerkMag-std()" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body gyroscope signal"
                                      )



#########################################################
# RENAME THE MEASUREMENT VARIABLES TO SOMETHING MORE MANAGABLE
#########################################################

names(combined_test_train_lbl)[2]<-"tbodyacc_mx"
names(combined_test_train_lbl)[3]<-"tbodyacc_my"
names(combined_test_train_lbl)[4]<-"tbodyacc_mz"

names(combined_test_train_lbl)[5]<-"tbodyacc_sx"  
names(combined_test_train_lbl)[6]<-"tbodyacc_sy"
names(combined_test_train_lbl)[7]<-"tbodyacc_sz" 
names(combined_test_train_lbl)[8]<-"tgravacc_mx" 

names(combined_test_train_lbl)[9]<-"tgravacc_my" 
names(combined_test_train_lbl)[10]<-"tgravacc_mz"
names(combined_test_train_lbl)[11]<-"tgravacc_sx"
names(combined_test_train_lbl)[12]<-"tgravacc_sy"

names(combined_test_train_lbl)[13]<-"tgravacc_sz"
names(combined_test_train_lbl)[14]<-"tbodyaccJ_mx"
names(combined_test_train_lbl)[15]<-"tbodyaccJ_my"
names(combined_test_train_lbl)[16]<-"tbodyaccJ_mz"

names(combined_test_train_lbl)[17]<-"tbodyaccJ_sx"
names(combined_test_train_lbl)[18]<-"tbodyaccJ_sy"
names(combined_test_train_lbl)[19]<-"tbodyaccJ_sz"
names(combined_test_train_lbl)[20]<-"tbodygyro_mx"

names(combined_test_train_lbl)[21]<-"tbodygyro_my"
names(combined_test_train_lbl)[22]<-"tbodygyro_mz"
names(combined_test_train_lbl)[23]<-"tbodygyro_sx"
names(combined_test_train_lbl)[24]<-"tbodygyro_sy"

names(combined_test_train_lbl)[25]<-"tbodygyro_sz"
names(combined_test_train_lbl)[26]<-"tbodygyroJ_mx"
names(combined_test_train_lbl)[27]<-"tbodygyroJ_my"
names(combined_test_train_lbl)[28]<-"tbodygyroJ_mz"

names(combined_test_train_lbl)[29]<-"tbodygyroJ_sx"
names(combined_test_train_lbl)[30]<-"tbodygyroJ_sy"
names(combined_test_train_lbl)[31]<-"tbodygyroJ_sz"
names(combined_test_train_lbl)[32]<-"tbodyaccmag_m"

names(combined_test_train_lbl)[33]<-"tbodyaccmag_s"
names(combined_test_train_lbl)[34]<-"tgravaccmag_m"
names(combined_test_train_lbl)[35]<-"tgravaccmag_s"
names(combined_test_train_lbl)[36]<-"tbodyaccJmag_m"

names(combined_test_train_lbl)[37]<-"tbodyaccJmag_s"
names(combined_test_train_lbl)[38]<-"tbodygyromag_m"
names(combined_test_train_lbl)[39]<-"tbodygyromag_s"
names(combined_test_train_lbl)[40]<-"tbodygyroJmag_m"

names(combined_test_train_lbl)[41]<-"tbodygyroJmag_s"
names(combined_test_train_lbl)[42]<-"fbodyacc_mx"
names(combined_test_train_lbl)[43]<-"fbodyacc_my"
names(combined_test_train_lbl)[44]<-"fbodyacc_mz"

names(combined_test_train_lbl)[45]<-"fbodyacc_sx"
names(combined_test_train_lbl)[46]<-"fbodyacc_sy"
names(combined_test_train_lbl)[47]<-"fbodyacc_sz"
names(combined_test_train_lbl)[48]<-"fbodyaccJ_mx"

names(combined_test_train_lbl)[49]<-"fbodyaccJ_my"
names(combined_test_train_lbl)[50]<-"fbodyaccJ_mz"
names(combined_test_train_lbl)[51]<-"fbodyaccJ_sx"
names(combined_test_train_lbl)[52]<-"fbodyaccJ_sy"

names(combined_test_train_lbl)[53]<-"fbodyaccJ_sz"
names(combined_test_train_lbl)[54]<-"fbodygyro_mx"
names(combined_test_train_lbl)[55]<-"fbodygyro_my"
names(combined_test_train_lbl)[56]<-"fbodygyro_mz"

names(combined_test_train_lbl)[57]<-"fbodygyro_sx"
names(combined_test_train_lbl)[58]<-"fbodygyro_sy"
names(combined_test_train_lbl)[59]<-"fbodygyro_sz"
names(combined_test_train_lbl)[60]<-"fbodyaccmag_m"

names(combined_test_train_lbl)[61]<-"fbodyaccmag_s"
names(combined_test_train_lbl)[62]<-"fbodyaccJmag_m"
names(combined_test_train_lbl)[63]<-"fbodyaccJmag_s"
names(combined_test_train_lbl)[64]<-"fbodygyromag_m"

names(combined_test_train_lbl)[65]<-"fbodygyromag_s"
names(combined_test_train_lbl)[66]<-"fbodygyroJmag_m"
names(combined_test_train_lbl)[67]<-"fbodygyroJmag_s"



###########################################################################
# GET THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY FOR EACH SUBJECT
###########################################################################

meands_1<-aggregate(formula = tbodyacc_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_2<-aggregate(formula = tbodyacc_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_3<-aggregate(formula = tbodyacc_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_4<-aggregate(formula = tbodyacc_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_5<-aggregate(formula = tbodyacc_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_6<-aggregate(formula = tbodyacc_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_7<-aggregate(formula = tgravacc_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_8<-aggregate(formula = tgravacc_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_9<-aggregate(formula = tgravacc_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_10<-aggregate(formula = tgravacc_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_11<-aggregate(formula = tgravacc_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_12<-aggregate(formula = tgravacc_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_13<-aggregate(formula = tbodyaccJ_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_14<-aggregate(formula = tbodyaccJ_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_15<-aggregate(formula = tbodyaccJ_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_16<-aggregate(formula = tbodyaccJ_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_17<-aggregate(formula = tbodyaccJ_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_18<-aggregate(formula = tbodyaccJ_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_19<-aggregate(formula = tbodygyro_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_20<-aggregate(formula = tbodygyro_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_21<-aggregate(formula = tbodygyro_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_22<-aggregate(formula = tbodygyro_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_23<-aggregate(formula = tbodygyro_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_24<-aggregate(formula = tbodygyro_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_25<-aggregate(formula = tbodygyroJ_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_26<-aggregate(formula = tbodygyroJ_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_27<-aggregate(formula = tbodygyroJ_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_28<-aggregate(formula = tbodygyroJ_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_29<-aggregate(formula = tbodygyroJ_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_30<-aggregate(formula = tbodygyroJ_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_31<-aggregate(formula = tbodyaccmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_32<-aggregate(formula = tbodyaccmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_33<-aggregate(formula = tgravaccmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_34<-aggregate(formula = tgravaccmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_35<-aggregate(formula = tbodyaccJmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_36<-aggregate(formula = tbodyaccJmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_37<-aggregate(formula = tbodygyromag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_38<-aggregate(formula = tbodygyromag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_39<-aggregate(formula = tbodygyroJmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_40<-aggregate(formula = tbodygyroJmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_41<-aggregate(formula = fbodyacc_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_42<-aggregate(formula = fbodyacc_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_43<-aggregate(formula = fbodyacc_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_44<-aggregate(formula = fbodyacc_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_45<-aggregate(formula = fbodyacc_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_46<-aggregate(formula = fbodyacc_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_47<-aggregate(formula = fbodyaccJ_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_48<-aggregate(formula = fbodyaccJ_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_49<-aggregate(formula = fbodyaccJ_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_50<-aggregate(formula = fbodyaccJ_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_51<-aggregate(formula = fbodyaccJ_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_52<-aggregate(formula = fbodyaccJ_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_53<-aggregate(formula = fbodygyro_mx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_54<-aggregate(formula = fbodygyro_my ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_55<-aggregate(formula = fbodygyro_mz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_56<-aggregate(formula = fbodygyro_sx ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_57<-aggregate(formula = fbodygyro_sy ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_58<-aggregate(formula = fbodygyro_sz ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_59<-aggregate(formula = fbodyaccmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_60<-aggregate(formula = fbodyaccmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_61<-aggregate(formula = fbodyaccJmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_62<-aggregate(formula = fbodyaccJmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_63<-aggregate(formula = fbodygyromag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)

meands_64<-aggregate(formula = fbodygyromag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_65<-aggregate(formula = fbodygyroJmag_m ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)
meands_66<-aggregate(formula = fbodygyroJmag_s ~ Activity_Character + Subject_ID, data=combined_test_train_lbl, FUN=mean)


final<-join_all(list(meands_1,meands_2,meands_3,meands_4,meands_5,
                     meands_6,meands_7,meands_8,meands_9,meands_10,
                     meands_11,meands_12,meands_13,meands_14,meands_15,
                     meands_16,meands_17,meands_18,meands_19,meands_20,
                     meands_21,meands_22,meands_23,meands_24,meands_25,
                     meands_26,meands_27,meands_28,meands_29,meands_30,
                     meands_31,meands_32,meands_33,meands_34,meands_35,
                     meands_36,meands_37,meands_38,meands_39,meands_40,
                     meands_41,meands_42,meands_43,meands_44,meands_45,
                     meands_46,meands_47,meands_48,meands_49,meands_50,
                     meands_51,meands_52,meands_53,meands_54,meands_55,
                     meands_56,meands_57,meands_58,meands_59,meands_60,
                     meands_61,meands_62,meands_63,meands_64,meands_65,
                     meands_66
                    ), by=c("Subject_ID","Activity_Character"), type='full')



########################
# ADD LABELS
########################

final_lbl<-apply_labels(final,
                    "tbodyacc_mx" = "The average of the mean of the time domain of the body acceleration signal along the X-axis",
                    "tbodyacc_my" = "The average of the mean of the time domain of the body acceleration signal along the Y-axis",           
                    "tbodyacc_mz" = "The average of the mean of the time domain of the body acceleration signal along the Z-axis",          
                    "tbodyacc_sx" = "The average of the standard deviation of the time domain of the body acceleration signal along the X-axis",             
                    "tbodyacc_sy" = "The average of the standard deviation of the time domain of the body acceleration signal along the Y-axis",           
                    "tbodyacc_sz" = "The average of the standard deviation of the time domain of the body acceleration signal along the Z-axis",            
                    "tgravacc_mx" = "The mean of the time domain of the gravity acceleration signal along the X-axis",     
                    "tgravacc_my" = "The mean of the time domain of the gravity acceleration signal along the Y-axis",           
                    "tgravacc_mz" = "The mean of the time domain of the gravity acceleration signal along the Z-axis",           
                    "tgravacc_sx" = "The standard deviation of the time domain of the gravity acceleration signal along the X-axis",           
                    "tgravacc_sy" = "The standard deviation of the time domain of the gravity acceleration signal along the Y-axis",       
                    "tgravacc_sz" = "The standard deviation of the time domain of the gravity acceleration signal along the Z-axis",        
                    "tbodyaccJ_mx" = "The mean of the time domain of the jerk signals of the body linear acceleration along the X-axis",      
                    "tbodyaccJ_my" = "The mean of the time domain of the jerk signals of the body linear acceleration along the Y-axis",        
                    "tbodyaccJ_mz" = "The mean of the time domain of the jerk signals of the body linear acceleration along the Z-axis",       
                    "tbodyaccJ_sx" = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the X-axis",         
                    "tbodyaccJ_sy" = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the Y-axis",         
                    "tbodyaccJ_sz" = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the Z-axis",         
                    "tbodygyro_mx" = "The mean of the time domain of the body gyroscope signal along the X-axis",           
                    "tbodygyro_my" = "The mean of the time domain of the body gyroscope signal along the Y-axis",         
                    "tbodygyro_mz" = "The mean of the time domain of the body gyroscope signal along the Z-axis",          
                    "tbodygyro_sx" = "The standard deviation of the time domain of the body gyroscope signal along the X-axis",          
                    "tbodygyro_sy" = "The standard deviation of the time domain of the body gyroscope signal along the Y-axis",          
                    "tbodygyro_sz" = "The standard deviation of the time domain of the body gyroscope signal along the Z-axis",           
                    "tbodygyroJ_mx" = "The mean of the time domain of the jerk signals of the body gyroscope signal along the X-axis",      
                    "tbodygyroJ_my" = "The mean of the time domain of the jerk signals of the body gyroscope signal along the Y-axis",        
                    "tbodygyroJ_mz" = "The mean of the time domain of the jerk signals of the body gyroscope signal along the Z-axis",        
                    "tbodygyroJ_sx" = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the X-axis",         
                    "tbodygyroJ_sy" = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the Y-axis",      
                    "tbodygyroJ_sz" = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the Z-axis",      
                    "tbodyaccmag_m" = "The mean of the time domain of the magnitude of the body linear acceleration",        
                    "tbodyaccmag_s" = "The standard deviation of the time domain of the magnitude of the body linear acceleration",           
                    "tgravaccmag_m" = "The mean of the time domain of the magnitude of the gravity acceleration signal",      
                    "tgravaccmag_s"  = "The standard deviation of the time domain of the magnitude of the gravity acceleration signal",       
                    "tbodyaccJmag_m" = "The mean of the time domain of the magnitude of the jerk signals of the body linear acceleration",    
                    "tbodyaccJmag_s" = "The standard deviation of the time domain of the magnitude of the jerk signals of the body linear acceleration",       
                    "tbodygyromag_m" = "The mean of the time domain of the magnitude of the body gyroscope signal",         
                    "tbodygyromag_s" = "The standard deviation of the time domain of the magnitude of the body gyroscope signal",           
                    "tbodygyroJmag_m" = "The mean of the time domain of the magnitude of the jerk signals of the body gyroscope signal",    
                    "tbodygyroJmag_s" = "The standard deviation of the time domain of the magnitude of the jerk signals of the body gyroscope signal",        
                    "fbodyacc_mx" = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the X-axis",          
                    "fbodyacc_my" = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the Y-axis",            
                    "fbodyacc_mz" = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the Z-axis",           
                    "fbodyacc_sx" = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the X-axis",              
                    "fbodyacc_sy" = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the Y-axis",              
                    "fbodyacc_sz" = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the Z-axis",              
                    "fbodyaccJ_mx" = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the X-axis",      
                    "fbodyaccJ_my" = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Y-axis",      
                    "fbodyaccJ_mz" = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Z-axis",      
                    "fbodyaccJ_sx" = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the X-axis",       
                    "fbodyaccJ_sy" = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Y-axis",       
                    "fbodyaccJ_sz" = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Z-axis",        
                    "fbodygyro_mx" = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the X-axis",         
                    "fbodygyro_my" = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Y-axis",           
                    "fbodygyro_mz" = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Z-axis",           
                    "fbodygyro_sx" = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the X-axis",            
                    "fbodygyro_sy" = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Y-axis",          
                    "fbodygyro_sz" = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Z-axis",          
                    "fbodyaccmag_m" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the body linear acceleration",        
                    "fbodyaccmag_s" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the body linear acceleration",           
                    "fbodyaccJmag_m" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body linear acceleration",   
                    "fbodyaccJmag_s" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body linear acceleration",  
                    "fbodygyromag_m" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the body gyroscope signal",   
                    "fbodygyromag_s" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the body gyroscope signal",         
                    "fbodygyroJmag_m" = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body gyroscope signal",   
                    "fbodygyroJmag_s" = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body gyroscope signal"
)



#########################
# SAVE DATASET
#########################

write.table(x=final_lbl, file="Average_of_Mean_Std_Meas_by_Subj_Activity.txt", row.name=FALSE)
