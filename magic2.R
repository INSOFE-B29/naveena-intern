rm(list=ls())

#library(pastecs)
#stat.desc(magicdata) 
library(readr)
magicdata <- read_csv("D:/insofe Mtrls/intern dataset/magic04.data.txt",col_names = FALSE, trim_ws = FALSE)
colnames(magicdata)<-c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans","fAlpha","fDist","class")
sum(is.na(magicdata))

str(magicdata)
library(caret)

    
magicdata$class<-ifelse(magicdata$class=="g",0,1)
table(magicdata$class)
magicdata$class<-as.factor(magicdata$class)
is.factor(magicdata$class)
library(corrplot)
corrplot(cor(magicdata[,-11]),method="number")


#pca,preprocess
library(caret)
preprocessParams <- preProcess(magicdata[,-11], method=c("center", "scale","pca"))
print(preprocessParams)

magicdata.pca<-predict(preprocessParams,magicdata[,-11])
mydata<-cbind(magicdata.pca,magicdata[,11])

#splitting train and test
set.seed(123)
train_rows<-sample(1:nrow(mydata),0.7*nrow(mydata))
train.data=mydata[train_rows,]
test.data=mydata[-train_rows,]
str(train.data)
str(test.data)

#modelbuilding svm
#install.packages("e1071")
library(e1071)
#install.packages("doMC")
#library(doMC)
#registerDoMC(cores=4)
model_svm <- svm(class ~ . , train.data,kernel ="linear",type='C-classification',scale=FALSE,cost=1,gamma=0.1)
library(car)
library(ROCR)
prob_train.svm <- predict(model_svm,type = "response")
pred <- prediction(as.numeric(prob_train.svm),as.numeric(train.data$class) )
perf <- performance(pred, measure="tnr", x.measure="fnr")
plot(perf, col=rainbow(10), colorize=T)
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)
pred_test.svm <- predict(model_svm,test.data,type = "response")
confusionMatrix(test.data$class,pred_test.svm)

#model_svm <- tune.svm(class~., data = train.data,kernel = "linear",cost=2^(1:2)) 
#preds.model=predict(model_svm,test.data,type="class")
#conf_matrix <- table(test.data$class, preds.model)
#print(conf_matrix)
#confusionMatrix(test.data$class,preds.model)

#roc for svm
library(ROCR)





  
#logistic
library(MASS)
log_reg <- glm(class~., data = train.data, family = binomial)
model_aic <- stepAIC(log_reg, direction = "both")
summary(model_aic)
summary(log_reg)
library(car)
vif(model_aic)
vif(log_reg)
#on train
prob_train <- predict(log_reg, type = "response")
library(ROCR)
pred <- prediction(prob_train, train.data$class)
perf <- performance(pred, measure="tnr", x.measure="fnr")
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
#auc
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)
preds.train<-ifelse(prob_train>0.6,1,0)
confusionMatrix(train.data$class,preds.train)





 