library(data.table)
library(xgboost)
library(FeatureHashing)
library(dummies)
library(caret)
library(h2o)
localh2o<-h2o.init(nthreads = -1)
train<-fread("train.csv",showProgress = T)
test<-fread("test.csv",showProgress = T)
str(train);dim(test)
dim(train)
summary(combin)
test[,loss:=mean(train$loss)]
c<-list(train,test)
combin<-rbindlist(c)
write.csv(combin,"combin.csv")
rm(combin)
#############
#############
table(combin$cat116)
combin[,prop.table(table(cat116))]
####look for pca in cat7,cat8,cat14,cat15,cat16,cat17,cat18,cat19,cat20,cat21,cat22,cat29
####cat32,cat33,cat34,cat35,cat39,cat42,cat43,cat45,cat46,cat47,cat48,cat51,cat52
####cat54,cat55,cat56,cat57,cat58,cat59,cat60,cat61,cat62,cat63,cat64,cat65,cat66
####cat67,cat69,cat70,cat74,cat76,cat77,cat78

#####

all_bar <- function(i){
  ggplot(combin,aes(x=i,y=loss))+
    geom_bar(stat = "identity",  color="black")+
    scale_fill_brewer(palette = "Pastel1")+
    theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

all_bar(combin$cat11)
 

tr <- function(a){
  ggplot(data = c_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =200) + geom_density()
  
}

dim(combin)
names(combin)
table(combin$loss)
tr(log(c_train$cont5))
####data to be used for PCA

prin_cat<-subset(combin,select = c(cat7,cat8,cat14,cat15,cat16,cat17,cat18,cat19,cat20,cat21,cat22,cat29,cat32,cat33,cat34,cat35,cat39,cat42,cat43,cat45,cat46,cat47,cat48,cat51,cat52,cat54,cat55,cat56,cat57,cat58,cat59,cat60,cat61,cat62,cat63,cat64,cat65,cat66,cat67,cat69,cat70,cat74,cat76,cat77,cat78))
prin_cat<-dummy.data.frame(prin_cat,sep="_")
pcaobject<-prcomp(prin_cat,center = T,scale=T)
percent_var<-pcaobject$sd^2/sum(pcaobject$sdev^2)*100
dim(prin_cat)
percent_var[1:50]
head(pcaobject$x[,1:5])
pca_cat<-data.frame(pcaobject$x[,1:46])
str(pca_cat)''
trans<-preProcess(prin_cat,method = c("BoxCox","pca"))
transformed<-predict(trans,prin_cat)
pca_data<-transformed[,c(1:42)]
dim(pca_data)
rm(transformed)
########################################
#####################################

myvar<-subset(combin,select =-c(cat7,cat8,cat14,cat15,cat16,cat17,cat18,cat19,cat20,cat21,cat22,cat29,cat32,cat33,cat34,cat35,cat39,cat42,cat43,cat45,cat46,cat47,cat48,cat51,cat52,cat54,cat55,cat56,cat57,cat58,cat59,cat60,cat61,cat62,cat63,cat64,cat65,cat66,cat67,cat69,cat70,cat74,cat76,cat77,cat78))
myvar$id<-NULL
dim(myvar);str(combin)
factvars<-myvar[,.SD,.SDcols=c(1:71)]
numvars<-myvar[,.SD,.SDcols=c(72:86)]
myvar[,(factcols):=lapply(.SD,factor),.SDcols=factcols]
str(factvars);str(numvars)
names(myvar)
myvar$cont13<-as.integer(myvar$cont13)

for(i in names(factvars)){
  p <- 5/100
  ld <- names(which(prop.table(table(factvars[[i]])) < p))
  levels(factvars[[i]])[levels(factvars[[i]]) %in% ld] <- "Other"
}
#####
factvars<-dummy.data.frame(factvars,sep="_")

##(variables_combin)
combin<-cbind(factvars,numvars)
write.csv(combin,"combin.csv")
combin<-fread("combin.csv")

c_test$loss<-NULL
c_train<-combin[1:188319,]
c_test<-combin[188319:313864,]
#c_test<-subset(c_test,select=-loss)
###c_train$V1=NULL
head(c_train)
dim(c_train)
c_test$loss<-NULL
names(c_train)
del<-setdiff(names(c_test),names(c_train))
dim(c_test)
####data to h20 cluster
train.h2o<-as.h2o(c_train)
test.h2o<-as.h2o(c_test)
####dependent variable(loss)
y.dep<-251
x.indep<-c(1:250)
dlearn.model<-h2o.deeplearning(y=y.dep,x=x.indep,training_frame = train.h2o,epoch=60,hidden = c(100,100),activation = "Rectifier",seed=123)
h2o.performance(gbm.model)
h2o.performance(dlearn.model)
predict.gbm<-as.data.frame(h2o.predict(gbm.model,test.h2o))
predict.dl<-as.data.frame(h2o.predict(dlearn.model,test.h2o))
write.csv(predict.dl,"dl_pred.csv")
write.csv(predict.gbm,"gbm_pred.csv")
h2o.varimp(gbm.model)
####################################################
####################################################
x_target<-c_train$loss
xgtrain<-xgb.DMatrix(data=as.matrix(c_train[,1:236,with=FALSE]),label=x_target)
xgtest<-xgb.DMatrix(data=as.matrix(c_test[,1:236,with=FALSE]))
#####xgboost
params<-list()
params$objective <- "reg:linear"
params$eta <- 0.1
params$max_depth <- 12
params$subsample <- 0.7
params$colsample_bytree <- 0.7
params$min_child_weight <- 80
params$eval_metric <- "rmse"


model_xgb_cv <- xgb.cv(params=params,xgtrain, nrounds = 10000,nfold = 5, early.stop.round = 30, prediction = TRUE,set.seed=123)
model_xgb<-xgb.train(params = params,xgtrain,nrounds = 169)
vimp<-xgb.importance(model=model_xgb,feature_names = names(c_test))
View(vimp)


pred<-predict(model_xgb,xgtest)
submit<-data.table(outcome=pred)
write.csv(submit,"model_10.csv")
names(c_train)
######lets remove 
lm_reg<-lm(loss~.,data=c_train)
summary(lm_reg)
names(c_train)

c_train<-subset(c_train,select=cat96_E-)
#c_train[]<-lapply(c_train,as.numeric)
str(c_train)
summary(c_train)
rm(c_test)
c_test<-subset(c_test,select=-cat96_E)
names(c_test)
summary(c_test$cont5)
##rm(prin_cat)
##c_train$cont5<-ifelse(c_train$cont5==0,0,1)
ax<-findCorrelation(x=cor(c_train),cutoff = 0.8)
c_train<-c_train[,-ax,with=F]
dim(c_train)
c_test<-c_test[,-ax,with=F]
dim(c_test)
write.csv(c_train,"c_train.csv")
write.csv(c_test,"c_test.csv")
write.csv(pca_data,"pca_data.csv")
rm(pca_data)





