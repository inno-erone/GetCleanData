remove(list=ls())


##  Set working 
if(file.exists( paste(getwd(), "/getdata_projectfiles_UCI HAR Dataset.zip", sep="")) ){
  
  #fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  #download.file(fileurl, destfile="./getdata_projectfiles_UCI HAR Dataset.zip")
  
  unzip("./getdata_projectfiles_UCI HAR Dataset.zip", files=NULL, overwrite=T)
  setwd(paste(getwd(), "/UCI HAR Dataset", sep=""))
} else {
  setwd(paste(getwd(), "/UCI HAR Dataset", sep=""))
}



## construct the test dataset
subject_test <- read.table(file="./test/subject_test.txt", as.is=T)
x_test <- read.table(file="./test/x_test.txt", as.is=T)
y_test <- read.table(file="./test/y_test.txt", as.is=T)
testDataset <- cbind(subject_test, y_test, x_test)
 

## construct the training dataset  
subject_train <- read.table(file="./train/subject_train.txt", as.is=T)
x_train <- read.table(file="./train/x_train.txt", as.is=T)
y_train <- read.table(file="./train/y_train.txt", as.is=T)
trainDataset <- cbind(subject_train, y_train, x_train)


xxx <- rbind(testDataset, trainDataset)
remove(list = setdiff(ls(), "xxx"))


## create appropriate/intiutive column. Note: signaldata column names are found in the features.txt file
## Also add an activity description to the bodyActivit dataFrame
features <- read.table(file="./features.txt", as.is=T)
features$V2 <- gsub("-", "_", features$V2)
features$V2 <- gsub("\\(|\\)|\\,", "", features$V2)

write.table(features, file="../new_features.txt", row.names=F, col.names=T, sep=" ")

activity_labels <- read.table(file="./activity_labels.txt", as.is=T)

names(xxx) <- c("subject", "activity", features[,2])



## maintain Only mesurements on MEAN:- mean()  and STANDARD DEVIATION: std()  on each mesurement
keep_names_like <- names(xxx)[grep("[Mm]ean|[Ss]td", names(xxx))]

df <- xxx[ , names(xxx) %in% c("subject", "activity", keep_names_like)]



vMeans <- NULL
vActivity <- NULL
vSubject <- NULL

for(i in 1:length(unique(df$subject))){  ## to track subject

  df_subset1 <- df[which(df$subject==i), ]
  
  for(j in 1:length(unique(df$activity))) {
    
    df_subset2 <- df_subset1[which(df_subset1$activity==j), ]
      
    if(nrow(df_subset2) != 0){
      computed_means <- colMeans(df_subset2[, 3:length(df)], na.rm=T)
      
      vSubject <- rbind(vSubject, i)
      vActivity <- rbind(vActivity, j)
      vMeans <- rbind(vMeans, computed_means) 
    } 
  }   
}

tidy <- suppressWarnings( data.frame(subject=vSubject, 
                                     activity=vActivity, 
                                     vMeans) )
names(tidy) <- names(df)


## Recode the activity data as it appears in the activity_labels file
tidy$activity <- as.character(tidy$activity)
tidy$activity <- gsub("1", "WALKING", tidy$activity)
tidy$activity <- gsub("2", "WALKING_UPSTAIRS", tidy$activity)
tidy$activity <- gsub("3", "WALKING_DOWNSTAIRS", tidy$activity)
tidy$activity <- gsub("4", "SITTING", tidy$activity)
tidy$activity <- gsub("5", "STANDING", tidy$activity)
tidy$activity <- gsub("6", "LAYING", tidy$activity)

write.table(tidy, file="../tidy_dataset.txt", row.names=F, col.names=T, sep=" ")
write.csv(tidy, file="../tidy_dataset.csv", row.names=F)
## Cleanup
## remove(list=ls())