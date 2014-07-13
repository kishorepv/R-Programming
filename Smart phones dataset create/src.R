#Some processsing to create a dataset (by merging the test and train data sets) and determine the means and standard deviation
#Refer the ReadMe for info about dataset

create <- function (character dir=".", character ansfile) {
## Create takes a working directory,dir and outputfile name ansfile as arguments
  
  
  oldwd = getwd() #store pwd to restore at the end
  setwd(dir) #sets working directory

# read the training data
  trdset <- read.table("x_train.txt", header = F)

#obtain the names for the above features	
  nam <- read.table("features.txt", header = F)

#feature names assigned to names attribute of trdset
  names(trdset) <- nam[,2]

#subject data
  trsub <- read.table("subject_train.txt", header = F)
  trdset$subject <- trsub$V1 #adds new column to the dataframe (table) trdset

#Y; the output 
  tract <- read.table("y_train.txt", header = F)

  trdset$activity <- tract$V1 #adds new column to the dataframe (table) trdset

#load the test data
  tedset <- read.table("x_test.txt", header = F)
  names(tedset) <- nam[, 2] #asssigning the names attribute	


  tesub <- read.table("subject_test.txt", header = F)
  tedset$subject <- tesub$V1 #adds new column to the dataframe (table) tedset

  teact <- read.table("y_test.txt", header = F)
  tedset$activity <- teact$V1 #adds new column to tedset


  dset <- rbind(trdset, tedset) #merging the training and test data sets

#grep for all feature names that are means/stds
  means <- grep(".*mean.*", nam[,2], val =T) 
  stds <- grep(".*std.*", nam[,2], val =T)
  meanstd <- c(means, ctds) # creates a vector of the above grep results

  subdset <- dset[, c(meanstd, "subject", "activity")] #extracts the means,stds,subject and activity cols only
  sp <- split(subdset, subdset$subject) #split based on the subject, one list element corrsponds to one test subject
  sp2 <- sapply(sp, function (x) { split(x, x$activity); }) #splits based on activity for each subject

  ans <- which(sp2, sapply(sp2, mean, meanstd)) # mean of each feature for each activity for each subject 


  write.csv(ans, ansfile) #writes the result to ansfile (2nd parameter)

  setwd(oldwd) #sets the working directory to old

}
