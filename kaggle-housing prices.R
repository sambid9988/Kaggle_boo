library(dplyr)
library(data.table)
library(caret)
library(ggplot2)
library(Hmisc)





house_train<-read.csv("train.csv",stringsAsFactors = F)
house_test<-read.csv("test.csv",stringsAsFactors = F)
str(house_train)
dim(house_train)
dim(house_test)
colSums(is.na(all_data))
all_data<-bind_rows(house_train,house_test)
dim(all_data)
View(all_data)
table(all_data$FireplaceQu)
table(all_data$MiscFeature)
#####OUTCOME VARIABLE IS SalePrice
str(all_data)
all_data<-subset(all_data,select=-c(Alley,PoolQC,Fence,MiscFeature))
all_data<-all_data[,-1]
######
numericals<-c()
dates<-all_data%>%
            select(YearBuilt,YearRemodAdd,GarageYrBlt,MoSold,YrSold)
colSums(is.na(dates))
str(dates);dim(dates)
View(dates)
write.csv(dates,"dates.csv")
######variables to be created in dates

######
numericals<-all_data%>%
            select(SalePrice,MiscVal,PoolArea,ScreenPorch,X3SsnPorch,
                   EnclosedPorch,WoodDeckSF,GarageCars,GarageArea,
                   Fireplaces,TotRmsAbvGrd,BedroomAbvGr,HalfBath,FullBath,
                   KitchenAbvGr,BsmtHalfBath,BsmtFullBath,GrLivArea,
                   LowQualFinSF,X1stFlrSF,X2ndFlrSF,TotalBsmtSF,
                   BsmtUnfSF,BsmtFinSF2,BsmtFinSF1,MasVnrArea,LotArea,LotFrontage)
dim(numericals)
View(numericals)
######
categoricals<-setdiff(names(all_data),names(cbind(dates,numericals)))
factors_var<-all_data[,categoricals]
str(factors);dim(factors)
factors[]<-lapply(factors,factor)
write.csv(factors,"factors.csv")

############EXPLORING DATA
######numericals first
View(cor(numericals,use = "complete.obs"))
colSums(is.na(numericals))
hist(log(numericals$LotFrontage),breaks=100)
boxplot(numericals$LotFrontage)
nearZeroVar(numericals)
highcor<-findCorrelation(x=cor(numericals),cutoff=0.75)
write.csv(numericals,"numericals.csv")
numericals<-numericals[,-highcor]
hist(log(numericals$TotalBsmtSF))
boxplot(log(numericals2$X1stFlrSF))
summary(numericals$TotRmsAbvGrd)
table(numericals2$BsmtFinSF1)
plot(numericals1$LotFrontage,numericals1$SalePrice)


######transform Lotarea(treat outliers first) then see for log transform
tr <- function(a){
  ggplot(data = numericals2, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + 
    geom_density()
  
}
tr(numericals2$LotFrontage)
summary(numericals2)
str(numericals2)
names(numericals1)
#numericals1<-numericals1[,-2]
#write.csv(numericals1,"numericals1")
colSums(is.na(numericals2))
View(numericals1)
numericals1<-numericals1[,-2]
#numericals1$WoodDeckSF<-as.factor(ifelse(numericals1$WoodDeckSF==0,0,1))
#numericals1$GarageCars<-ifelse(numericals1$GarageCars==5,3,numericals1$GarageCars)
#numericals1$GarageCars<-as.factor(numericals1$GarageCars)
#numericals1$GarageArea[numericals1$GarageArea==0]<-NA
#numericals1$X2ndFlrSF[numericals1$X2ndFlrSF==0]<-NA
#numericals1$TotalBsmtSF[numericals1$TotalBsmtSF==0]<-NA
#numericals1$BsmtUnfSF[numericals1$BsmtUnfSF==0]<-NA
#num_areg<-aregImpute(~GarageArea+X2ndFlrSF+TotalBsmtSF+BsmtUnfSF+MasVnrArea+LotFrontage,data = numericals1,n.impute = 5)
#num_areg$imputed$LotFrontage
#num_areg_data<-impute.transcan(num_areg,imputation = 1,data = numericals1,list.out = T,pr=F,check = F)
#numericals2<-cbind(numericals1,num_areg_data)
#names(numericals2)
#numericals2<-numericals2[,-c(4,12,13,14,16,18)]
#numericals2$GarageCars<-as.factor(numericals2$GarageCars)
#numericals2$GarageCars[is.na(numericals2$GarageCars)]<-mean(numericals2$GarageCars,na.rm = T)

numericals2$GarageArea<-ifelse(numericals2$GarageArea>index,index,numericals2$GarageArea)
numericals2$Fireplaces<-ifelse(numericals2$Fireplaces==4,1,numericals2$Fireplaces)
numericals2$Fireplaces<-as.factor(numericals2$Fireplaces)
numericals2$TotRmsAbvGrd<-ifelse(numericals2$TotRmsAbvGrd==15,3,numericals2$TotRmsAbvGrd)
numericals2$TotRmsAbvGrd<-as.factor(numericals2$TotRmsAbvGrd)
numericals2$HalfBath<-as.factor(ifelse(numericals2$HalfBath==2,1,numericals2$HalfBath))
numericals2$FullBath<-ifelse(numericals2$FullBath==4,1,numericals2$FullBath)
numericals2$FullBath<-as.factor(numericals2$FullBath)
numericals2$BsmtFullBath<-ifelse(numericals2$BsmtFullBath==3,1,numericals2$BsmtFullBath)
numericals2$BsmtFullBath<-as.factor(numericals2$BsmtFullBath)
numericals2$X1stFlrSF<-log(numericals2$X1stFlrSF)

numericals2$X2ndFlrSF<-sqrt(numericals2$X2ndFlrSF)
numericals2$TotalBsmtSF<-log(numericals2$TotalBsmtSF)
numericals2$BsmtUnfSF<-sqrt(numericals2$BsmtUnfSF)
numericals2$BsmtFinSF1<-as.factor(ifelse(numericals2$BsmtFinSF1>0,1,numericals2$BsmtFinSF1))
numericals2$MasVnrArea<-as.factor(ifelse(numericals2$MasVnrArea>0,1,numericals2$MasVnrArea))
numericals2$LotArea<-log(numericals2$LotArea)
index<-80+1.5*IQR(numericals2$LotFrontage)
numericals2$LotFrontage<-ifelse(numericals2$LotFrontage>index,1,numericals2$LotFrontage)
###################################

summary(dates)
dates_features$quarter<-ifelse(dates_features$quarter==12,4,dates_features$quarter)
dates_features$quarter<-as.factor(dates_features$)
str(dates1)
View(dates_features)
dates_features<-dates1%>%
  mutate(recent_year=2010-YearBuilt,
         diff_builtsold=YearBuilt-YrSold,
         diff_builtremod=YearRemodAdd-YearBuilt,
         recent_garageblt=2010-GarageYrBlt,
         diff_garbuilt=YearBuilt-GarageYrBlt,
         diff_yrsolgar=YearRemodAdd-YrSold,
         yr_soldfromtwoten=2010-YrSold)
dates1<-dates
str(dates_features)
cor(dates_features$diff_garbuilt,numericals2$SalePrice,use = "complete.obs")
dates_features<-dates_features[,-4]
plot(dates_features$recent_garageblt,numericals2$SalePrice)
hist(dates_features$recent_garageblt)
dates_features$recent_garageblt<-sqrt(dates_features$recent_garageblt)
View(dates1)
dates
colSums(is.na(dates_features))
plot(dates1$GarageYrBlt,numericals2$SalePrice)
View(dates1)
dates1<-kNN(dates,k=7)
dim(dates_features)
dates_features<-na.omit(dates_features)
#nearZeroVar(dates1)
dates1<-dates1
log_model<-lm(numericals2$SalePrice~.,data=dates_features)
summary(log_model)
summary(dates1)
dates1<-subset(dates1,select =-c(YearBuilt_imp :YrSold_imp))
rm(house_train)
##############################################################
###############################################################
##############################################################
write.csv(dates_features,"dates_feat.csv")
write.csv(numericals2,"numericals2.csv")
##############################
str(factors)
colSums(is.na(factors))
table(factors$SaleCondition)
#factors<-factors[,-3]
factors%>%
  group_by(SaleCondition)%>%
  summarise(m=mean(SalePrice,na.rm=T))%>%
  arrange(desc(m))
  


levels(factors$BsmtFinType1)
View(factors)
factors$OpenPorchSF<-factors_var$OpenPorchSF
factors$OpenPorchSF<-ifelse(factors$OpenPorchSF>0,1,0)

#####
factors$BsmtFinType1<-as.factor(factors$BsmtFinType1)
#levels(factors$MSZoning)
factors$MSZoning<-as.character(factors$MSZoning)
factors$SaleType[is.na(factors$SaleType)]<-"Oth"

levels(factors$SaleCondition)[levels(factors$SaleCondition)=="Alloca"]<-"Normal"
plot(factors$LandContour,factors$SalePrice)
#factors<-factors[,-9]

ggplot(data=factors,aes(x=GarageType,y=SalePrice))+
  geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=SalePrice), vjust=1.6, color="white", size=1.5)+
  theme_minimal()
names(factors)
factors<-factors[,-25]
#factors<-factors[,-5]
write.csv(factors,"factors2.csv")###till exterior1st
#####MSSubClass 
str(numericals2)
str(dates_features)
str(factors)
numericals2<-numericals2[,-1]
traindata<-bind_cols(numericals2,dates_features)
traindata<-bind_cols(traindata,factors)
trainingdata<-slice(traindata,1:1460)
testingdata<-traindata[1461:nrow(traindata),]
colSums(is.na(testingdata))
View(testingdata)
mycontrol<-trainControl(method="cv",number=10,verboseIter = T)
lmdata<-trainingdata
names(lmdata)
rf_model<-train(SalePrice~.,data=dum_data,method="mlpSGD",metric="RMSE",trControl=mycontrol)
rf_model
summary(rf_model)
varImp(rf_model)
##########dropping varaibles GarageType.type1,GarageType.type3,GarageFinish.RFn
##########MasVnrType.None,GarageFinish.Unf,MasVnrType.Stone,LotConfig.Other,HeatingQC.TA
##########HeatingQC.Gd  ,LotConfig.Inside,OpenPorchSF,MasVnrType.Brk,GarageArea
#########RoofStyle.other,RoofStyle.Hip,MasVnrArea.0,recent_garageblt,SaleType.Oth
###########LotShape.Ireg ,GarageCond.TA,SaleType.WD ,GarageCond.Po,Foundation.CBlock  
#########Foundation.PConc,MasVnrArea.1,LotShape.Reg,TotRmsAbvGrd.1,TotRmsAbvGrd.3 
######Electrical.other,ExterCond.TA,BsmtFullBath.0 ,ExterCond.Gd,quarter,Electrical.SBrkr  
#####BsmtFullBath.1,WoodDeckSF.0,LotConfig.Corner,RoofStyle.Gable,BsmtCond.Gd ,MSSubClass.old
########BsmtCond.TA,MSSubClass.new,BsmtQual.TA,BsmtQual.Gd,PavedDrive.N,recentyear
#####PavedDrive.Y,FireplaceQu.Gd,FireplaceQu.TA,BsmtFinType2.good,BsmtFinType2.bad 
#########CentralAir.N,BedroomAbvGr 
prediction<-predict(rf_model,testingdata)
View(data.frame(prediction))

prediction1<-data.frame(prediction)
write.csv(prediction1,"prediction1.csv")

dum_var1<-dummyVars(~.,data=trainingdata)
train_data<-data.frame(predict(dum_var1,trainingdata))
testingdata<-testingdata[,-61]
dim(train_data)
names(dum_data)
str(train)
#dum_data<-dum_data[,-5]
#lm.fit<-lm(SalePrice~.,data=dum_data)
#summary(lm.fit)
#vif(lm.fit)
library(car)
####################
library(xgboost)
test_data<-test_data[,-c(93,68,82,65,33,79,107,125,72,69)]

  
target<-train_data$SalePrice
train_data<-data.matrix(train_data[,-127])
test_data<-data.matrix(test_data)
zeros <- rep(0, 1459)

#--------------------------------------------
# I will fit 50 models.
# The predictions are averaged out.
# So this is simply an ensemble of boosters.
#--------------------------------------------

control <- 50

for (i in 1:control){
  
  bst <- xgboost(data =train_data,
                 label = target,
                 eta = 0.1,
                 max_depth = 6,
                 subsample = 0.5,
                 colsample_bytree = 1,
                 nrounds = 400,
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 maximize = FALSE)
  
  yhat <- predict(bst,test_data)
  zeros <- zeros + yhat
}


zeros <- zeros/control
submission<-data.frame(zeros,stringsAsFactors = F)
View(submission)
write.csv(submission,"submission.csv")
importance<-xgb.importance(model=bst)
importance
xgb.plot.importance(importance_matrix =importance)


