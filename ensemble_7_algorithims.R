library(caret)
library(dplyr)
library(caretEnsemble)
train<-read.csv("train.csv")
test<-read.csv("test.csv")
View(train)
dim(train);dim(test)
str(train);,str(test)
train<-train[,-1]
test<-test[,-1]
train_index<-createDataPartition(train$species,p=0.70,list = F)
training<-train[train_index,]
testing<-train[-train_index,]


######
set.seed(123)
mycontrol<-trainControl(method="cv",number=10,classProbs = T,verboseIter = T)
slda_model<-train(species~.,data=training,method="slda",preProcess=c("zv","center","scale","BoxCox"),trControl=mycontrol)
slda_model

set.seed(123)
pda_model<-train(species~.,data=training,method="pda",preProcess=c("center","scale","BoxCox"),trControl=mycontrol)
pda_model

set.seed(123)
pam_model<-train(species~.,data=training,method="pam",trControl=mycontrol,preProcess=c("center","scale","BoxCox"))
pam_model

set.seed(123)
hdda_model<-train(species~.,data=training,method="hdda",trControl=mycontrol,preProcess=c("center","scale","BoxCox"))
hdda_model

set.seed(123)
sda_model<-train(species~.,data=training,method="sda",trControl=mycontrol,preProcess=c("center","scale"))
sda_model

set.seed(123)
mda_model<-train(species~.,data=training,method="mda",trControl=mycontrol,preProcess=c("center","scale","BoxCox"))
mda_model

set.seed(123)
knn_model<-train(species~.,data=training,method="kknn",trControl=mycontrol,preProcess=c("center","scale"))
knn_model

set.seed(123)
amdai_model<-train(species~.,data=training,method="amdai",trControl=mycontrol,preProcess=c("center","scale","pca"))
amdai_model

set.seed(123)
smda_model<-train(species~.,data=training,method="smda",trControl=mycontrol,pr

set.seed(123)




resample_list<-resamples(list(SLDA=slda_model,PDA=pda_model,PAM=pam_model,HDDA=hdda_model,SDA=sda_model,MDA=mda_model))
bwplot(resample_list,metric="Accuracy")
View(modelCor(resample_list))


p1<-predict(slda_model,test,preProcess=c("center","scale","BoxCox"),type="prob")
p2<-predict(pda_model,test,preProcess=c("center","scale","BoxCox"),type="prob")
p3<-predict(pam_model,test,preProcess=c("center","scale","BoxCox"),type="prob")
p4<-predict(hdda_model,test,preProcess=c("center","scale","BoxCox"),type="prob")
p5<-predict(sda_model,test,preProcess=c("center","scale","BoxCox"),type="prob")
p6<-predict(mda_model,test,preProcess=c("center","scale","BoxCox"),type="prob")
p7<-predict(knn_model,test,preProcess=c("center","scale"),type="prob")
p8<-predict(amdai_model,test,preProcess=c("center","scale"),type="prob")



sample<-read.csv("sample_submission.csv")
p_all<-(p1+p2+p3+p4+p5+p6+p7+p8)/8
submission<-data.frame(id=sample$id,p_all)
write.csv(submission,"submit.csv",row.names = F)
