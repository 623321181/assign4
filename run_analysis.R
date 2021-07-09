  library(dplyr)
  library(plyr)
  
  features <- read.table('features.txt')
  activities <- read.table('activity_labels.txt')
  xtest <- read.table('test/X_test.txt')
  ytest <- read.table('test/y_test.txt')
  subject_test <- read.table('test/subject_test.txt')
  xtrain <- read.table('train/X_train.txt')
  ytrain <- read.table('train/y_train.txt')
  subject_train <- read.table('train/subject_train.txt')
  
  #changing column names
  collabels <- features[,2]
  colnames(xtest) <- collabels
  colnames(xtrain) <- collabels
  
  ytest <- inner_join(ytest, activities)
  ytrain <- inner_join(ytrain, activities)
  
  #merging data sets
  test <- cbind(xtest, ytest[,2])
  test <- cbind(test, subject_test)
  train <- cbind(xtrain, ytrain[,2])
  train<- cbind(train, subject_train)
  colnames(test)[562] <- c("activity")
  colnames(train)[562] <- c("activity")
  colnames(test)[563] <- c('subject')
  colnames(train)[563] <- c('subject')
  data <- rbind(test, train)
  
  #extract means and stds
  
  columnnames <- colnames(data)
  means_and_stds <- grep('mean|std', columnnames)
  columns <- c(means_and_stds, 562, 563)
  data <- data[,columns]
  
  freq <- grep('Freq', columnnames)
  data <- data[,-freq]
  
  
  #get tidy data names
  names(data) <- gsub('mean()', 'Mean value', names(data))
  names(data) <- gsub('std()', 'Standard deviation', names(data))
  
  #create groups and create a new table
  
  data_grouped <- group_by(data, activity, subject)
  summarized_data <- summarise_all(data_grouped, funs(mean))
  write.table(summarized_data, file = 'final data.txt', row.name = FALSE)
