baseUrl <- "./UCI HAR Dataset/"

# function that takes test/train data row and return the related activity label
getActivityName <- function(row){
  return (activity_labels[activity_labels$V1 == row[2], "V2"]) 
}

# function that takes the type = "test|train" and a file name, then return the full file name  
getFileNameForType <- function(type, name){
  fileName <- type
  fileName <- paste(fileName , "/", sep = "")
  fileName <- paste(fileName , name, sep = "")
  fileName <- paste(fileName , "_", sep = "")
  fileName <- paste(fileName , type, sep = "")
  fileName <- paste(fileName , ".txt", sep = "")
  return (fileName)
}

# function that reads "test|train" data
readData <- function(type = "test"){

  
  # read features titles
  filePath <- paste(baseUrl , "features.txt", sep = "")
  features <- read.table(filePath)
  
  # read activity_labels
  filePath <- paste(baseUrl , "activity_labels.txt", sep = "")
  activity_labels <- read.table(filePath)

  # read test data
  filePath <- paste(baseUrl , getFileNameForType(type, "X"), sep = "") 
  x_test <- read.table(filePath)
  
  # read activity column of test data
  filePath <- paste(baseUrl , getFileNameForType(type, "y"), sep = "")
  y_test <- read.table(filePath)
  
  # read subject column of test data
  filePath <- paste(baseUrl , getFileNameForType(type, "subject"), sep = "")
  subject_test <- read.table(filePath)
  
  # set names to test data 
  colnames(x_test) <- features$V2
  
  # select only mean and standard deviation columns
  selectedComunsNames <- grep("mean|std", features$V2, value = TRUE)
  data <- x_test[, selectedComunsNames]
  
  # add activity column to test data
  data <- cbind(activity=y_test$V1, data)

  # add subject column to test data
  data <- cbind(subject=subject_test$V1, data)
   
  # Uses descriptive activity names to name the activities in the test data set
  data$activity <- apply(data, 1,getActivityName)
  
  return(data)

}

# read test data
testData <- readData("test")

# read train data
trainData <- readData("train")

# merge test and train data
allData <- rbind(testData, trainData)

# group the data by subject and activity
aggData <- aggregate(allData, by=list(bySubject = allData$subject, byActivity  = allData$activity),  FUN=mean, na.rm=TRUE)

# drop repeated columns
drops <- c("subject", "activity")
aggData <- aggData[ , !(names(aggData) %in% drops)]

View(aggData)











