library(plyr)
library(doBy)

X_test<-read.table("x_test.txt")
y_test<-read.table("y_test.txt")
X_train<-read.table("x_train.txt")
y_train<-read.table("y_train.txt")
subject_train<-read.table("subject_train.txt")
subject_test<-read.table("subject_test.txt")

features<-read.table("features.txt")
activity<-read.table("activity_labels.txt")

temp1<-as.matrix(y_test)
temp2<-as.matrix(y_train)

y_test$V1<-as.factor(y_test$V1)
levels(y_test$V1)<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
y_train$V1<-as.factor(y_train$V1)
levels(y_train$V1)<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

names(X_test)<-features$V2
names(X_train)<-features$V2

class1 <- grep("mean", features$V2 , ignore.case=TRUE, fixed=FALSE)
class2 <- grep("std", features$V2 , ignore.case=TRUE, fixed=FALSE)
class <- union(class1,class2)
class<-sort(class)
name<-features$V2[class]

body_acc_x_train<-read.table("body_acc_x_train.txt")
body_acc_y_train<-read.table("body_acc_y_train.txt")
body_acc_z_train<-read.table("body_acc_z_train.txt")

body_gyro_x_train<-read.table("body_gyro_x_train.txt")
body_gyro_y_train<-read.table("body_gyro_y_train.txt")
body_gyro_z_train<-read.table("body_gyro_z_train.txt")

total_acc_x_train<-read.table("total_acc_x_train.txt")
total_acc_y_train<-read.table("total_acc_y_train.txt")
total_acc_z_train<-read.table("total_acc_z_train.txt")

body_acc_x_test<-read.table("body_acc_x_test.txt")
body_acc_y_test<-read.table("body_acc_y_test.txt")
body_acc_z_test<-read.table("body_acc_z_test.txt")

body_gyro_x_test<-read.table("body_gyro_x_test.txt")
body_gyro_y_test<-read.table("body_gyro_y_test.txt")
body_gyro_z_test<-read.table("body_gyro_z_test.txt")

total_acc_x_test<-read.table("total_acc_x_test.txt")
total_acc_y_test<-read.table("total_acc_y_test.txt")
total_acc_z_test<-read.table("total_acc_z_test.txt")

y_test<-cbind(y_test,subject_test)
y_train<-cbind(y_train,subject_train)

test<-cbind(y_test,X_test)
train<-cbind(y_train,X_train)

train<-cbind(train,body_acc_x_train)
train<-cbind(train,body_acc_y_train)
train<-cbind(train,body_acc_z_train)
train<-cbind(train,body_gyro_x_train)
train<-cbind(train,body_gyro_y_train)
train<-cbind(train,body_gyro_z_train)
train<-cbind(train,total_acc_x_train)
train<-cbind(train,total_acc_y_train)
train<-cbind(train,total_acc_z_train)

test<-cbind(test,body_acc_x_test)
test<-cbind(test,body_acc_y_test)
test<-cbind(test,body_acc_z_test)
test<-cbind(test,body_gyro_x_test)
test<-cbind(test,body_gyro_y_test)
test<-cbind(test,body_gyro_z_test)
test<-cbind(test,total_acc_x_test)
test<-cbind(test,total_acc_y_test)
test<-cbind(test,total_acc_z_test)

v<-c(1:nrow(test))

for (i in v){
  v[i] <- "Test"
  }

test<-cbind(v,test)

v<-c(1:nrow(train))

for (i in v){
  v[i] <- "Train"
}

train<-cbind(v,train)

Data<-rbind(train, test)

names(Data)[1]<-paste("Type")
names(Data)[2]<-paste("Activity")
names(Data)[3]<-paste("Subject")

finalclass<-class + 3
finalclass<-union(c(1,2,3),finalclass)

FilteredData<-Data[,finalclass] 

output<- summaryBy(FilteredData[,4:10] ~ Activity + Subject, data=FilteredData, FUN=c(mean))

for (i in 4:ncol(FilteredData)){
  temp<-summaryBy(FilteredData[,i] ~ Activity + Subject, data=FilteredData, FUN=c(mean))
  output <- cbind(output,temp[, 3])
}
names(output)[3:ncol(output)]<-names(FilteredData)[4:ncol(FilteredData)]
write.table(output, "output.txt", sep="\t")