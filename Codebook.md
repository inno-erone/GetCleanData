Data dictionary - tidy_dataset
===============================
-------   

**subject**   
Datatype   :  integer (1 - 30)    
An identifier of the subject who carried out the experiment. Identifiers are as below:-      

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
## [24] 24 25 26 27 28 29 30
```
          
**activity**     
    Datatype : character   
    An activity in which the the subject was involved in and mesurements were recorded.  
    

```
##   code activity_description
## 1    1              WALKING
## 2    2     WALKING_UPSTAIRS
## 3    3   WALKING_DOWNSTAIRS
## 4    4              SITTING
## 5    5             STANDING
## 6    6               LAYING
```



**Other**  
   Datatype : double:-      
* Time domain signals are prefix ***'t'*** to denote time.   
* Frequency domain signals are prefixed with ***'f'*** to denote frequency.   
* Time body linear acceleration and angular velocity derived from Jerk signals are coded as (tBodyAccJerk_XYZ and tBodyGyroJerk_XYZ).   
* The magnitude of three-dimensional signals were calculated using the Euclidean norm and coded as:-   
  (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

These three parameters reflect in the variable names as below:-


```
##  [1] "tBodyAcc_mean_X"                  
##  [2] "tBodyAcc_mean_Y"                  
##  [3] "tBodyAcc_mean_Z"                  
##  [4] "tBodyAcc_std_X"                   
##  [5] "tBodyAcc_std_Y"                   
##  [6] "tBodyAcc_std_Z"                   
##  [7] "tGravityAcc_mean_X"               
##  [8] "tGravityAcc_mean_Y"               
##  [9] "tGravityAcc_mean_Z"               
## [10] "tGravityAcc_std_X"                
## [11] "tGravityAcc_std_Y"                
## [12] "tGravityAcc_std_Z"                
## [13] "tBodyAccJerk_mean_X"              
## [14] "tBodyAccJerk_mean_Y"              
## [15] "tBodyAccJerk_mean_Z"              
## [16] "tBodyAccJerk_std_X"               
## [17] "tBodyAccJerk_std_Y"               
## [18] "tBodyAccJerk_std_Z"               
## [19] "tBodyGyro_mean_X"                 
## [20] "tBodyGyro_mean_Y"                 
## [21] "tBodyGyro_mean_Z"                 
## [22] "tBodyGyro_std_X"                  
## [23] "tBodyGyro_std_Y"                  
## [24] "tBodyGyro_std_Z"                  
## [25] "tBodyGyroJerk_mean_X"             
## [26] "tBodyGyroJerk_mean_Y"             
## [27] "tBodyGyroJerk_mean_Z"             
## [28] "tBodyGyroJerk_std_X"              
## [29] "tBodyGyroJerk_std_Y"              
## [30] "tBodyGyroJerk_std_Z"              
## [31] "tBodyAccMag_mean"                 
## [32] "tBodyAccMag_std"                  
## [33] "tGravityAccMag_mean"              
## [34] "tGravityAccMag_std"               
## [35] "tBodyAccJerkMag_mean"             
## [36] "tBodyAccJerkMag_std"              
## [37] "tBodyGyroMag_mean"                
## [38] "tBodyGyroMag_std"                 
## [39] "tBodyGyroJerkMag_mean"            
## [40] "tBodyGyroJerkMag_std"             
## [41] "fBodyAcc_mean_X"                  
## [42] "fBodyAcc_mean_Y"                  
## [43] "fBodyAcc_mean_Z"                  
## [44] "fBodyAcc_std_X"                   
## [45] "fBodyAcc_std_Y"                   
## [46] "fBodyAcc_std_Z"                   
## [47] "fBodyAcc_meanFreq_X"              
## [48] "fBodyAcc_meanFreq_Y"              
## [49] "fBodyAcc_meanFreq_Z"              
## [50] "fBodyAccJerk_mean_X"              
## [51] "fBodyAccJerk_mean_Y"              
## [52] "fBodyAccJerk_mean_Z"              
## [53] "fBodyAccJerk_std_X"               
## [54] "fBodyAccJerk_std_Y"               
## [55] "fBodyAccJerk_std_Z"               
## [56] "fBodyAccJerk_meanFreq_X"          
## [57] "fBodyAccJerk_meanFreq_Y"          
## [58] "fBodyAccJerk_meanFreq_Z"          
## [59] "fBodyGyro_mean_X"                 
## [60] "fBodyGyro_mean_Y"                 
## [61] "fBodyGyro_mean_Z"                 
## [62] "fBodyGyro_std_X"                  
## [63] "fBodyGyro_std_Y"                  
## [64] "fBodyGyro_std_Z"                  
## [65] "fBodyGyro_meanFreq_X"             
## [66] "fBodyGyro_meanFreq_Y"             
## [67] "fBodyGyro_meanFreq_Z"             
## [68] "fBodyAccMag_mean"                 
## [69] "fBodyAccMag_std"                  
## [70] "fBodyAccMag_meanFreq"             
## [71] "fBodyBodyAccJerkMag_mean"         
## [72] "fBodyBodyAccJerkMag_std"          
## [73] "fBodyBodyAccJerkMag_meanFreq"     
## [74] "fBodyBodyGyroMag_mean"            
## [75] "fBodyBodyGyroMag_std"             
## [76] "fBodyBodyGyroMag_meanFreq"        
## [77] "fBodyBodyGyroJerkMag_mean"        
## [78] "fBodyBodyGyroJerkMag_std"         
## [79] "fBodyBodyGyroJerkMag_meanFreq"    
## [80] "angletBodyAccMeangravity"         
## [81] "angletBodyAccJerkMeangravityMean" 
## [82] "angletBodyGyroMeangravityMean"    
## [83] "angletBodyGyroJerkMeangravityMean"
## [84] "angleXgravityMean"                
## [85] "angleYgravityMean"                
## [86] "angleZgravityMean"
```

```
## NULL
```


