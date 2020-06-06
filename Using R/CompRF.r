rm(list=ls())
cat("\14")

library(randomForest)

Comp<-read.csv("E:/itsstudytym/assignments/Random Forest/Company_Data.csv")

Comp$Urban<-as.numeric(Comp$Urban)
Comp$US<-as.numeric(Comp$US)
Comp$ShelveLoc<-as.numeric(Comp$ShelveLoc)

normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

Comp_Norm<-as.data.frame(lapply(Comp[,2:11],normalize))
Comp_New<-cbind(Comp$Sales,Comp_Norm)
names(Comp_New)[names(Comp_New)=='Comp$Sales']<-'Sales'
Comp_New$Sales<-ifelse(Comp_New$Sales>7.490,1,0)
Comp_New$Sales<-as.factor(Comp_New$Sales)

##create model
model<-randomForest(Sales~.,data=Comp,ntree=1000)

print(model)
print(importance(model))

pred<-predict(model,Comp[,-1])
table(Comp_New$Sales,pred)
