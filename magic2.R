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



  
#logistic
library(MASS)
log_reg <- glm(class~., data = train.data, family = binomial)
model_aic <- stepAIC(log_reg, direction = "both")
summary(model_aic)
summary(log_reg)
library(car)
vif(model_aic)
vif(log_reg)
# roc on train
prob_train <- predict(log_reg, type = "response")
library(ROCR)
pred <- prediction(prob_train, train.data$class)
perf <- performance(pred, measure="tnr", x.measure="fnr")
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
#auc
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)
#Choose a Cutoff Value
#Predictions on test data
prob_test <- predict(log_reg, test.data, type = "response")
preds_test <- ifelse(prob_test > 0.65, "1", "0")
#preds.train<-ifelse(prob_train>0.65,1,0)
confusionMatrix(test.data$class,preds_test)


#SVM kernel="anovadot"
library(kernlab)
model_svm <- ksvm(class ~ . , train.data,kernel="anovadot")
pred<-predict(model_svm,test.data)
#tanhdot(scale = 1, offset = 1)
confusionMatrix(pred,test.data$class)

#SVM kernel="tanhdot"
#train svm using custom kernel
model<-ksvm(class~.,data=train.data,kernel="tanhdot", C=5,cross=5)
pred<-predict(model_svm,test.data)
str(pred)
table(pred)
#tanhdot(scale = 1, offset = 1)
confusionMatrix(pred,test.data$class)

#prioir_svm <- tune.svm(formul=class~.,data=train.data, cost =0.1 , gamma =0.4,tunecontrol = tc)

#decision tree CART
#tc <- tune.control(cross = 5)
library(rpart)
#Model the tree
dc_tree.model <- rpart(class~.,train.data,control = rpart.control(cp =0.0005))
summary(dc_tree.model)
#Tree Explicability
#The variable importance can accessed accessing variable.importance from the tree list
dc_tree.model$variable.importance
library(rpart.plot)
rpart.plot(dc_tree.model)
pred.dc_model <- predict(dc_tree.model, test.data)
#control = rpart.control(minsplit = 20, xval = 81, cp = 0.01)
str(pred.dc_model)
preds_tree <- ifelse(pred.dc_model[, 1] > pred.dc_model[, 2], 0, 1)
confusionMatrix(preds_tree,test.data$class)

#finding cp value
dtCart=rpart(class ~.,data=train.data,method="class")
cp=data.frame(printcp(dtCart))
cp[cp$xstd==min(cp$xstd),][1,]$CP

#c5.0 moel building
library(C50)

c5_tree <- C5.0(class ~ . , train.data,cp=1,ntrees=1500)
c5_rules <- C5.0(class ~ . , train.data,rules=T)
#Variable Importance in trees
C5imp(c5_tree, metric = "usage")
summary(c5_rules)
plot(c5_tree)
preds <- predict(c5_tree, test.data)
library(caret)
confusionMatrix(preds, test.data$class)


#Bagged Decision Trees
library(ipred)
set.seed(1234)

model_tree_bag <- bagging(class ~ . , data=train.data, control = rpart.control(cp=.0001))
preds_tree_bag <- predict(model_tree_bag, test.data)
confusionMatrix(preds_tree_bag, test.data$class)
#dtCart=rpart(class ~.,data=train.data,method="class")
#cp=data.frame(printcp(dtCart))
#cp[cp$xstd==min(cp$xstd),][1,]$CP
#overfitting testing 
library(caret)
preds_tree_bag <- predict(model_tree_bag, train.data)
confusionMatrix(preds_tree_bag, train.data$class)

#xgboost
library(vegan)
library(dummies)
library(xgboost)
names(train.data)
#dtrain = xgb.DMatrix(data = as.matrix(train.data[-8]), label =as.factor(train.data$class))
#model = xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2,objective = "binary:logistic", verbose = 1)

xgmodel = xgboost(data = as.matrix(train.data[-8]), label = as.numeric(train.data$class), nrounds = 200)
str(train.data)
#testing on train data 
y_pred = predict(xgmodel, newdata = as.matrix(train.data[-8]))
table (train.data$class,ifelse(y_pred >= 1.5,1,0))
#table(train.data$class)
caret::confusionMatrix(train.data$class,ifelse(y_pred >= 1.5,1,0))

#testing on test data 
y_pred = predict(xgmodel, newdata = as.matrix(test.data[-8]))
table (test.data$class,ifelse(y_pred >= 1.5,1,0))
caret::confusionMatrix(test.data$class,ifelse(y_pred >= 1.5,1,0))

#xgboost on magic data with out pca
str(magicdata)
#spliting in to train and test on magic data w.o pca
set.seed(123)
train_rows.md<-sample(1:nrow(magicdata),0.7*nrow(magicdata))
train.data.md=magicdata[train_rows.md,]
test.data.md=magicdata[-train_rows.md,]
str(train.data.md)
str(test.data.md)
#xgboost model bunding on magic w.o pca
xgmodel = xgboost(data = as.matrix(train.data.md[-11]), label = as.numeric(train.data.md$class), nrounds = 200)
str(train.data.md)
#testing on train data 
y_pred = predict(xgmodel, newdata = as.matrix(train.data.md[-11]))
table (train.data.md$class,ifelse(y_pred >= 1.6,1,0))
table(train.data.md$class)
caret::confusionMatrix(train.data$class,ifelse(y_pred >= 1.6,1,0))

#testing on test data 
y_pred = predict(xgmodel, newdata = as.matrix(test.data.md[-11]))
table (test.data.md$class,ifelse(y_pred >= 1.6,1,0))
table(test.data.md$class)
caret::confusionMatrix(test.data.md$class,ifelse(y_pred >= 1.6,1,0))

#model building on rf
library(randomForest)
model_rf <- randomForest(train.data$class ~ . , train.data,ntree=1500,mtry=8)
importance(model_rf)
varImpPlot(model_rf)
preds_rf <- predict(model_rf, test.data)
confusionMatrix(preds_rf, test.data$class)

# model building on rf with out pca 
model_rf <- randomForest(train.data.md$class ~ . , train.data.md,ntree=2000,mtry=8)
importance(model_rf)
varImpPlot(model_rf)
preds_rf <- predict(model_rf, test.data.md)
confusionMatrix(preds_rf, test.data.md$class)



