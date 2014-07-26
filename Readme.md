Getting and Cleaning Data - Assignment
=====================================
----

## Preamble   
The dataset is a summary of the mean of MEANS and STANDARD DEVIATIONS of an experiment carried out with a group of 30 volunteers within an age bracket of 19-48 years.      

Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist.   

This code compiles and transforms the original datasets contained in a zip file and outputs the results in the file ***tidy_dataset.txt*** and  ***tidy_dataset.csv*** stored in you working directry.    


## Instructions:   

1. Set your working directory as you desire with the **setwd()** command.   


2. Place the __getdata_projectfiles_UCI HAR Dataset.zip__ as downloaded from the assignment page.      
  If for some reason this file name was edited, REPLACE all occurances of the text   

 "getdata_projectfiles_UCI HAR Dataset" **with**  "you_new_zipName"   
  
3. The codebook comes in two versions.   
 * The already knitted ready-to-use codebook.html file.   
 * The original script that generates the codebook. ***ONLY and ONLY*** run the Codebook.rmd script after the **run_analysis.r**.   
 This is so because the required files needed to knit the codebook are generated by the run_analysis.r script.   


## Considerations:  
1.  All variable with the text mean or std were conisidered for this assignment.   

 grep("[Mm]ean|[Ss]td", names(xxx))   producing a dataset with dimensions (10229 X 86)   

2. The final dataset(tidy_dataset) is a 180X88 285Kb file that storea in you working directory   

Have fun. \o/   

