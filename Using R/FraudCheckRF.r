rm(list=ls())
cat("\14")

library(randomForest)
library(DMwR)
library(caret)
library(C50)

FCheck<-read.csv("E:/itsstudytym/assignments/Random Forest/Fraud_check.csv")
FCheck$Taxable.Income <-  ifelse(FCheck$Taxable.Income <= 30000,'Risky','Good')
FCheck$Undergrad<-as.numeric(FCheck$Undergrad)
FCheck$Marital.Status<-as.numeric(FCheck$Marital.Status)
FCheck$Urban<-as.numeric(FCheck$Urban)
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

FCheck_Norm<-as.data.frame(lapply(FCheck[,-3],normalize))
FCheck<-cbind(FCheck$Taxable.Income,FCheck_Norm)
names(FCheck)[names(FCheck)=='FCheck$Taxable.Income']<-'Taxable.Income'

set.seed(7)
inTraininglocal<-createDataPartition(FCheck$Taxable.Income,p=.70,list=F)
training<-FCheck[inTraininglocal,]
testing<-FCheck[-inTraininglocal,]

table(training$Taxable.Income)
round(prop.table(table(training$Taxable.Income)) * 100, digits = 1)


##undersampling
good<-which(training$Taxable.Income=="Good")
good_data<-training[good,]
risky_data<-training[-good,]
intraingood<-createDataPartition(good_data$Taxable.Income,p=.25,list=F)
good_data1<-good_data[intraingood,]
finaltraindata<-rbind.data.frame(good_data1,risky_data)

model<-randomForest(Taxable.Income~.,data=finaltraindata,ntree=5000)
print(model)

##SMOTE
balance_data<-SMOTE(Taxable.Income~.,training,perc.over = 300,k=5,perc.under = 50)
as.data.frame(table(balance_data$Taxable.Income))
round(prop.table(table(balance_data$Taxable.Income)) * 100, digits = 1)

model<-randomForest(Taxable.Income~.,data=balance_data,ntree=1000)
print(model)
print(importance(model))

pred<-predict(model,testing[,-1])
a<-table(testing$Taxable.Income,pred)

