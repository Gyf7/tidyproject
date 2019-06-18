library(dplyr)
library(stringr)

#make train's table
sub_train <- read.table("subject_train.txt", sep = " ", header = F, stringsAsFactors = F)
train_data <- read.table("X_train.txt", stringsAsFactors = F)
features <- read.table("features.txt", colClasses = character(), sep = " ")
#simplified featuresnames
nfeatures <- gsub("_","",features[,2])
nfeatures <- gsub("-","",nfeatures)
nfeatures <- gsub(",","",nfeatures)
nfeatures <- gsub("[()]", "", nfeatures)
nfeatures <- str_replace_all(nfeatures,"^f","frequency")
nfeatures <- str_replace_all(nfeatures,"^t","time")
nfeatures <- tolower(nfeatures)
names(train_data) <- nfeatures
act_train <- read.table("y_train.txt", sep = " ", header = F, stringsAsFactors = F)
sub_act_train <- cbind(sub_train,act_train)
names(sub_act_train) <- list("subject","activity")
train_t <- cbind(sub_act_train,train_data)

#create test's table
sub_test <- read.table("subject_test.txt", sep = " ", header = F, stringsAsFactors = F)
test_data <- read.table("X_test.txt", stringsAsFactors = F)
names(test_data) <- nfeatures
act_test <- read.table("y_test.txt", sep = " ", header = F, stringsAsFactors = F)
sub_act_test <- cbind(sub_test,act_test)
names(sub_act_test) <- list("subject","activity")
test_t <- cbind(sub_act_test,test_data)

#merge train and test table
all_t <- rbind(train_t,test_t)

#convert activity collumn to character
all_t$activity <- as.character(all_t$activity)
#replace number by activity name
b <- all_t$activity
b <- str_replace_all(b,"1","WALKING")
b <- str_replace_all(b,"2","WALKING_UPSTAIRS")
b <- str_replace_all(b,"3","WALKING_DOWNSTAIRS")
b <- str_replace_all(b,"4","SITTING")
b <- str_replace_all(b,"5","STANDING")
b <- str_replace_all(b,"6","LAYING")
all_t$activity <- b
rm(b)

#indext column that contain mean or std
ind_mean <- grep("mean",names(all_t))
ind_std <- grep("std",names(all_t))
ind <- c(c(1,2),ind_mean,ind_std)
rm(ind_mean,ind_std)
sort(ind)
final_t <- all_t[ind]

#convert table to numeric
i <- 3
while (i<ncol(final_t)){
  y[,i] <- as.numeric(y[,i])
  i <- i+1
}

#use dplyr to group by subject / activity
final_by_subject <- group_by(final_t,subject)
final_by_activity <- group_by(final_t,activity)

#summarise final 2 table mean for each subject and activity
result1 <- summarise_all(final_by_activity, mean)
result2 <- summarise_all(final_by_subject, mean)
end <- rbind(result1,result2)
write.table(end,file = "end.txt", row.names = F)

