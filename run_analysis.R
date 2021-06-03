library(dplyr)
library(data.table)

### 1.MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET. ###

feature_names <- read.table(file='UCI HAR Dataset\\features.txt')[,2]

# 1.1. Create function to create 1 or 2 data tables (cbind|rbind if needed)
create_table <- function(X,var_names,y=FALSE, var_names_2,rbin=FALSE,...){
    table <- fread(file=X,col.names = var_names,...)
    if (y != FALSE){
        table2 <- fread(file=y,col.names = var_names_2)
        if (rbin != FALSE){
            table <- rbind(table,table2)
            return(table)}
        table <- cbind(table,table2)
        return(table)}
}
# 1.2. Call the function and combining tables into 1 table
train_set<-create_table('UCI HAR Dataset\\train\\X_train.txt',feature_names,
                         'UCI HAR Dataset\\train\\y_train.txt','activities')
test_set<-create_table('UCI HAR Dataset\\test\\X_test.txt',feature_names,
                        'UCI HAR Dataset\\test\\y_test.txt','activities')

subject <- create_table('UCI HAR Dataset\\train\\subject_train.txt','subject',
                        'UCI HAR Dataset\\test\\subject_test.txt','subject',
                        rbin=TRUE)

full_data <- rbind(train_set,test_set)
full_data <- cbind(subject,full_data)

### 2. EXTRACTS ONLY THE MEASUREMENTS ###
### ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT. ###

# Search through names with regex to subset the full_data
mean_sd_col <- grep('subject|mean|std|activities',names(full_data),value=TRUE)
mean_sd_data <- select(full_data,mean_sd_col)

### 3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET ###

activity_label <- read.table(file='UCI HAR Dataset\\activity_labels.txt')[,2]
# Replace values of mean_sd_data$activities with above list
for (i in c(1:6)){
    mean_sd_data <- mutate(mean_sd_data,
                           activities = replace(activities, activities == i,
                                                activity_label[i]))
}
### 4.APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES. ###
# Already done in 1.1

### 5. FROM THE DATA SET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY DATA SET ### 
### WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT. ###

# 5.1. Create a group of activities and subject, 
# then calculate the mean of all column except the group. Then write result
tidy_dataset <- mean_sd_data[,lapply(.SD,mean), by = .(activities, subject)]

write.table(tidy_dataset,'tidy_dataset.txt',row.name=FALSE)