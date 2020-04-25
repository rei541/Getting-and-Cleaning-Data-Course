========================================================================
run_analysis.R
Version 1.0

========================================================================
Author: Christine Yip

========================================================================
Purpose:
Cleans the Human Activity Recognition Using Smartphones Dataset
from www.smartlab.ws, keeping only the mean and standard deviations
of the measures and transforms it into a tidy dataset 
as defined by Hadley Wickham (http://vita.had.co.nz/papers/tidy-data.pdf)
to be then used to create another tidy dataset containing the
average of the mean and standard deviation measures by
subject and activity.

========================================================================
Note:
Make sure all the datasets from the Human Activity Recognition Using Smartphones
are in the working directory. This is also where the final dataset will be saved.

========================================================================
Special Note about Tidy dataset:
This script "tidies" the data by making sure that the resulting dataset 
follows the three rules identified by Wickham:

1. Each variable forms a column.
2. Each observation forms a row.
3. Each type of observational unit forms a table.

========================================================================
Code Steps:

1. Read in the relevant packages.

2. Read in the datasets 
	(activity_labels, features, subject_test, subject_train, x_test, x_train, y_test, y_train).

3. Replace the default column names with the more descriptive variable names from the features dataset.

4. Identify which features have the mean and standard deviation of the measurements.
	(Note: I defined it by getting the features with "mean()" or "std()" in the name.) 
	       
5. For x_test and x_train, select the x_test and x_train datasets to only include those 
	measurement variables with the mean and standard deviation.

6. Update the y_test and y_train datasets with the actual names of the activities using the activity_labels dataset.

7. Rename the subject variable in the subject_test and subject_train datasets.

8. Combined and stack together the subject_test, subject_train, x_test, x_train, y_test, and y_train
	into one combined dataset.

9. Add descriptive labels to the combined dataset.

10. Rename the measurement variables so they are more manageable than the names from the features dataset.

11. Get the average of each measurement variable for each activity for each subject. 

12. Add the labels to the final dataset.

13. Save the dataset to the working directory.




========================================================================
Codebook:

Subject_ID = "The subject identification number",
Activity_Character = "The activity the subject engaged in",
tbodyacc_mx = "The average of the mean of the time domain of the body acceleration signal along the X-axis",
tbodyacc_my = "The average of the mean of the time domain of the body acceleration signal along the Y-axis",           
tbodyacc_mz = "The average of the mean of the time domain of the body acceleration signal along the Z-axis",          
tbodyacc_sx = "The average of the standard deviation of the time domain of the body acceleration signal along the X-axis",             
tbodyacc_sy = "The average of the standard deviation of the time domain of the body acceleration signal along the Y-axis",           
tbodyacc_sz = "The average of the standard deviation of the time domain of the body acceleration signal along the Z-axis",            
tgravacc_mx = "The mean of the time domain of the gravity acceleration signal along the X-axis",     
tgravacc_my = "The mean of the time domain of the gravity acceleration signal along the Y-axis",           
tgravacc_mz = "The mean of the time domain of the gravity acceleration signal along the Z-axis",           
tgravacc_sx = "The standard deviation of the time domain of the gravity acceleration signal along the X-axis",           
tgravacc_sy = "The standard deviation of the time domain of the gravity acceleration signal along the Y-axis",       
tgravacc_sz = "The standard deviation of the time domain of the gravity acceleration signal along the Z-axis",        
tbodyaccJ_mx = "The mean of the time domain of the jerk signals of the body linear acceleration along the X-axis",      
tbodyaccJ_my = "The mean of the time domain of the jerk signals of the body linear acceleration along the Y-axis",        
tbodyaccJ_mz = "The mean of the time domain of the jerk signals of the body linear acceleration along the Z-axis",       
tbodyaccJ_sx = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the X-axis",         
tbodyaccJ_sy = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the Y-axis",         
tbodyaccJ_sz = "The standard deviation of the time domain of the jerk signals of the body linear acceleration along the Z-axis",         
tbodygyro_mx = "The mean of the time domain of the body gyroscope signal along the X-axis",           
tbodygyro_my = "The mean of the time domain of the body gyroscope signal along the Y-axis",         
tbodygyro_mz = "The mean of the time domain of the body gyroscope signal along the Z-axis",          
tbodygyro_sx = "The standard deviation of the time domain of the body gyroscope signal along the X-axis",          
tbodygyro_sy = "The standard deviation of the time domain of the body gyroscope signal along the Y-axis",          
tbodygyro_sz = "The standard deviation of the time domain of the body gyroscope signal along the Z-axis",           
tbodygyroJ_mx = "The mean of the time domain of the jerk signals of the body gyroscope signal along the X-axis",      
tbodygyroJ_my = "The mean of the time domain of the jerk signals of the body gyroscope signal along the Y-axis",        
tbodygyroJ_mz = "The mean of the time domain of the jerk signals of the body gyroscope signal along the Z-axis",        
tbodygyroJ_sx = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the X-axis",         
tbodygyroJ_sy = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the Y-axis",      
tbodygyroJ_sz = "The standard deviation of the time domain of the jerk signals of the body gyroscope signal along the Z-axis",      
tbodyaccmag_m = "The mean of the time domain of the magnitude of the body linear acceleration",        
tbodyaccmag_s = "The standard deviation of the time domain of the magnitude of the body linear acceleration",           
tgravaccmag_m = "The mean of the time domain of the magnitude of the gravity acceleration signal",      
tgravaccmag_s  = "The standard deviation of the time domain of the magnitude of the gravity acceleration signal",       
tbodyaccJmag_m = "The mean of the time domain of the magnitude of the jerk signals of the body linear acceleration",    
tbodyaccJmag_s = "The standard deviation of the time domain of the magnitude of the jerk signals of the body linear acceleration",       
tbodygyromag_m = "The mean of the time domain of the magnitude of the body gyroscope signal",         
tbodygyromag_s = "The standard deviation of the time domain of the magnitude of the body gyroscope signal",           
tbodygyroJmag_m = "The mean of the time domain of the magnitude of the jerk signals of the body gyroscope signal",    
tbodygyroJmag_s = "The standard deviation of the time domain of the magnitude of the jerk signals of the body gyroscope signal",        
fbodyacc_mx = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the X-axis",          
fbodyacc_my = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the Y-axis",            
fbodyacc_mz = "The mean of the Fast Fourier Transform (FFT) on the body acceleration signal along the Z-axis",           
fbodyacc_sx = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the X-axis",              
fbodyacc_sy = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the Y-axis",              
fbodyacc_sz = "The standard deviation of the Fast Fourier Transform (FFT) on the body acceleration signal along the Z-axis",              
fbodyaccJ_mx = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the X-axis",      
fbodyaccJ_my = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Y-axis",      
fbodyaccJ_mz = "The mean of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Z-axis",      
fbodyaccJ_sx = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the X-axis",       
fbodyaccJ_sy = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Y-axis",       
fbodyaccJ_sz = "The standard deviation of the Fast Fourier Transform (FFT) of the jerk signals of the body linear acceleration along the Z-axis",        
fbodygyro_mx = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the X-axis",         
fbodygyro_my = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Y-axis",           
fbodygyro_mz = "The mean of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Z-axis",           
fbodygyro_sx = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the X-axis",            
fbodygyro_sy = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Y-axis",          
fbodygyro_sz = "The standard deviation of the Fast Fourier Transform (FFT) of the body gyroscope signal along the Z-axis",          
fbodyaccmag_m = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the body linear acceleration",        
fbodyaccmag_s = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the body linear acceleration",           
fbodyaccJmag_m = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body linear acceleration",   
fbodyaccJmag_s = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body linear acceleration",  
fbodygyromag_m = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the body gyroscope signal",   
fbodygyromag_s = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the body gyroscope signal",         
fbodygyroJmag_m = "The mean of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body gyroscope signal",   
fbodygyroJmag_s = "The standard deviation of the Fast Fourier Transform (FFT) of the magnitude of the jerk signals of the body gyroscope signal"



