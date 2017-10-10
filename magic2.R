rm(list=ls())

#library(pastecs)
#stat.desc(magicdata) 
library(readr)
magicdata <- read_csv("D:/insofe Mtrls/intern dataset/magic04.data.txt",col_names = FALSE, trim_ws = FALSE)
colnames(magicdata)<-c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans","fAlpha","fDist","class")
sum(is.na(magicdata))

str(magicdata)
library(caret)
#unique(magicdata$class)
magicdata$class<-ifelse(magicdata$class=="g",0,1)
table(magicdata$class)
magicdata$class<-as.factor(magicdata$class)
is.factor(magicdata$class)
library(corrplot)
corrplot(cor(magicdata[,-11]),method="number")
par(mfrow = c(2,2))
plot(magicdata$fConc,magicdata$fConc1,main = "fconc vs fconc1")

table(magicdata$class)
#pca,preprocess
library(caret)
preprocessParams <- preProcess(magicdata[,-11], method=c("center", "scale","pca"))
print(preprocessParams)
magicdata.pca<-predict(preprocessParams,magicdata[,-11])
mydata<-cbind(magicdata.pca,magicdata[,11])
str(mydata)

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
pred_test.svm <- predict(model_svm,test.data)
confusionMatrix(test.data$class,pred_test.svm)$overall[1]
confusionMatrix(test.data$class,pred_test.svm)
preds_train_svm<-predict(model_svm)

#model build w.o pca
svm_wopca<-svm(class~.,train.data.md,kernel="linear",type='C-classification',cost=20)
pred<-predict(svm_wopca,test.data.md)
confusionMatrix(pred,test.data.md$class)
preds_train_svmwopca<-predict(svm_wopca)
#model_svm <- tune.svm(class~., data = train.data,kernel = "linear",cost=2^(1:2)) 
#preds.model=predict(model_svm,test.data,type="class")
#conf_matrix <- table(test.data$class, preds.model)
#print(conf_matrix)
#confusionMatrix(test.data$class,preds.model)

#svm with cv
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_fit <- train(class~., data = train.data, method = "svmLinear", trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
preds.svm<-predict(svm_fit,test.data)
confusionMatrix(preds.svm,test.data$class)
  
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
prob_test <- predict(log_reg, test.data,type = "response")
preds_test <- ifelse(prob_test > 0.65, "1", "0")
confusionMatrix(test.data$class,preds_test)
#prediction on train 
preds_train_log<-predict(log_reg,train.data,type = "response")
table(preds_train_log)
preds_train_lpgprob <- ifelse(preds_train_log >0.65, "1", "0")
confusionMatrix(preds_train_lpgprob,train.data$class)


#logestic wo pca
log_reg.wopca <- glm(class~., data = train.data.md, family = binomial)
model_aic.wopca <- stepAIC(log_reg.wopca, direction = "both")
summary(model_aic.wopca)
summary(log_reg.wopca)
library(car)
vif(model_aic.wopca)
vif(log_reg.wopca)
prob_train.wopca <- predict(log_reg.wopca, type = "response")
library(ROCR)
pred.wopca <- prediction(prob_train.wopca, train.data.md$class)
perf.wopca <- performance(pred.wopca, measure="tnr", x.measure="fnr")
plot(perf.wopca, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
#auc
perf_auc.wopca <- performance(pred.wopca, measure="auc")
auc.wopca <- perf_auc.wopca@y.values[[1]]
print(auc.wopca)
#Choose a Cutoff Value
#Predictions on test data
prob_test.wopca <- predict(log_reg.wopca, test.data.md, type = "response")
preds_test.wopca <- ifelse(prob_test.wopca > 0.7, "1", "0")
#preds.train<-ifelse(prob_train>0.65,1,0)
confusionMatrix(test.data.md$class,preds_test.wopca)

#let's now remove the "fconc" variable, as it is the lesser significant of the two
#magicdatafcon<-magicdata
#str(magicdatafcon)
#str(magicdata)
#cor(magicdata$fConc,magicdata$fConc1)
#modellgcor<- glm(class~., data = train.data.md[,-4], family = binomial)
#str(modellgcor)
#pred.fcon<-predict(modellgcor,test.data.md)
#test.data.md$fConc<-NULL
#str(test.data.md)
#confusionMatrix(pred.fcon,test.data.md[,-4]$class)


#SVM kernel="anovadot"
library(kernlab)
model_svm.ano <- ksvm(class ~ . , train.data,kernel="anovadot")
pred.ano<-predict(model_svm.ano,test.data)
#tanhdot(scale = 1, offset = 1)
confusionMatrix(pred.ano,test.data$class)$overall[1]
confusionMatrix(pred.ano,test.data$class)
preds_train_ano<-predict(model_svm.ano)

#SVM kernel="anovadot" w.o pca
model.svm.wopca <- ksvm(class ~ . , train.data.md,kernel="anovadot",sigma = 3, degree = 4)
pred<-predict(model.svm.wopca,test.data.md)
confusionMatrix(pred,test.data$class)
preds_train_svmwopca<-predict(model.svm.wopca)


#SVM kernel="tanhdot"
#train svm using custom kernel
model.tanhdot<-ksvm(class~.,data=train.data,kernel="tanhdot", C=5,cross=5)
pred.tan<-predict(model.tanhdot,test.data)
str(pred)
table(pred)
#tanhdot(scale = 1, offset = 1)
confusionMatrix(pred.tan,test.data$class)$overall[1]
confusionMatrix(pred.tan,test.data$class)
preds_train_tanhdot<-predict(model.tanhdot)
#prioir_svm <- tune.svm(formul=class~.,data=train.data, cost =0.1 , gamma =0.4,tunecontrol = tc)

#decision tree CART
library(rpart)
#Model the tree
dc_tree.model <- rpart(class~.,train.data,control = rpart.control(cp =0.01))
pred.dc_tree<-predict(dc_tree.model,test.data)
prob.dc_tree <- ifelse(pred.dc_tree[, 1] > pred.dc_tree[, 2], 0, 1)
confusionMatrix(prob.dc_tree,test.data$class)
confusionMatrix(prob.dc_tree,test.data$class)$overall[1]
summary(dc_tree.model)
#Tree Explicability
#The variable importance can accessed accessing variable.importance from the tree list
dc_tree.model$variable.importance
library(rpart.plot)
rpart.plot(dc_tree.model)
#test on train data 
dc_tree.model <- rpart(class~.,train.data,control = rpart.control(cp =0.01))
pred.dc_tree<-predict(dc_tree.model,train.data)
prob.dc_tree <- ifelse(pred.dc_tree[, 1] > pred.dc_tree[, 2], 0, 1)
confusionMatrix(prob.dc_tree,train.data$class)
#finding cp value
dtCart=rpart(class ~.,data=train.data,method="class")
cp=data.frame(printcp(dtCart))
cp[cp$xstd==min(cp$xstd),][1,]$CP


#dt with cv
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dt_fit <- train(class~., data = train.data, method = "rpart", trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
preds.fit.dc<-predict(dt_fit,test.data)
confusionMatrix(preds.fit.dc,test.data$class)


#c5.0 moel building
library(C50)
c5_tree <- C5.0(class ~ . , train.data,cp=1,ntrees=1500)
c5_rules <- C5.0(class ~ . , train.data,rules=T)
#Variable Importance in trees
C5imp(c5_tree, metric = "usage")
summary(c5_rules)
plot(c5_tree)
preds.c5 <- predict(c5_tree, test.data)
library(caret)
confusionMatrix(preds.c5, test.data$class)$overall[1]
confusionMatrix(preds.c5, test.data$class)




#Bagged Decision Trees
library(ipred)
set.seed(1234)
model_tree_bag <- bagging(class ~ . , data=train.data, cp=.0001)


preds_tree_bag <- predict(model_tree_bag, test.data)
confusionMatrix(preds_tree_bag, test.data$class)
#dtCart=rpart(class ~.,data=train.data,method="class")
#cp=data.frame(printcp(dtCart))
#cp[cp$xstd==min(cp$xstd),][1,]$CP
#overfitting testing 
library(caret)
preds_tree_bag <- predict(model_tree_bag, train.data)
confusionMatrix(preds_tree_bag, train.data$class)
preds_train_bdt<-predict(model_tree_bag)

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
model_rf <- randomForest(train.data$class ~ . , train.data,ntree=1500)
importance(model_rf)
varImpPlot(model_rf)
preds_rf <- predict(model_rf, test.data)
confusionMatrix(preds_rf, test.data$class)$overall[1]
confusionMatrix(preds_rf, test.data$class)

preds_train_rf<-predict(model_rf)

# model building on rf with out pca 
model_rf <- randomForest(train.data.md$class ~ . , train.data.md,ntree=2000,mtry=8)
importance(model_rf)
varImpPlot(model_rf)
preds_rf <- predict(model_rf, test.data.md)
confusionMatrix(preds_rf, test.data.md$class)

#rf with cv
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "Accuracy"
set.seed(123)
mtry <- sqrt(ncol(magicdata))
str(magicdata)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(class~., data=train.data.md, method="rf", tuneGrid=tunegrid, trControl=trctrl,preProcess = c("center", "scale"))
preds.rf.cv<-predict(rf_default,test.data.md)
confusionMatrix(preds.rf.cv,test.data.md$class)
print(rf_default)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(123)
mtry <- sqrt(ncol(magicdata))
rf_random <- train(class~., data=train.data.md, method="rf", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
preds.rf.cv<-predict(rf_random,test.data.md)
confusionMatrix(preds.rf.cv,test.data.md$class)


#knn 
library(class)
library(DMwR)
library(caret)
model_knn <- knn3(class~.,train.data, k =5)
preds <- predict(model_knn,test.data)
#library(caret)
preds_knn <- ifelse(preds[, 1] > preds[, 2], 0, 1)
confusionMatrix(preds_knn,test.data$class)



#knn wo pca
model_knnwopca <- knn3(class~.,train.data.md, k =9,l = 0)

predswopca<-predict(model_knnwopca,test.data.md)
preds_knnwopca <- ifelse(predswopca[, 1] > predswopca[, 2], 0, 1)
table(preds_knnwopca)
confusionMatrix(preds_knnwopca,test.data.md$class)



#knn
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(class~., data = train.data, method = "knn", trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
#We can see variation in Accuracy w.r.t K value by plotting these in a graph.
plot(knn_fit)
test_pred <- predict(knn_fit, newdata = test.data)
table(test_pred)
confusionMatrix(test_pred,test.data$class)

#knn wo pca 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(class~., data = train.data.md, method = "knn", trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
#We can see variation in Accuracy w.r.t K value by plotting these in a graph.
plot(knn_fit)
test_pred <- predict(knn_fit, newdata = test.data.md)
table(test_pred)
confusionMatrix(test_pred,test.data$class)


#ANN
train.x = data.matrix(train.data[,-8])
train.y = as.numeric(as.character(train.data[,8]))
str(train.y)
test.x = data.matrix(test.data[,-8])
test.y = as.numeric(as.character(test.data[,8]))
install.packages("mxnet")
library(mxnet)
mx.set.seed(0)
Sys.time() -> start
model <- mx.mlp(train.x, train.y, hidden_node=c(10), out_node=2, activation="tanh", out_activation="softmax",
                num.round=10, array.batch.size=100, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.accuracy)
Sys.time() -> end
paste(end - start)
preds = predict(model, test.x)
preds=t(preds)
pred.label = ifelse(preds[,2]<0.55, 0, 1)
library(DMwR)
library(caret)
confusionMatrix(pred.label,test.y)

 
#ann with out pca
train.x = data.matrix(train.data.md[,-11])
train.y = as.numeric(as.character(train.data.md[,11]))
str(train.data.md)
str(train.y)
sum(is.na(train.data.md))
test.x = data.matrix(test.data.md[,-11])
test.y = as.numeric(as.character(test.data.md[,11]))
library(mxnet)
mx.set.seed(0)
Sys.time() -> start
model <- mx.mlp(train.x, train.y, hidden_node=c(10), out_node=2, activation="tanh", out_activation="softmax",
                num.round=10, array.batch.size=100, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.accuracy)
Sys.time() -> end
paste(end - start)
preds = predict(model, test.x)
preds=t(preds)
pred.label = ifelse(preds[,2]<0.55, 0, 1)
library(DMwR)
library(caret)
confusionMatrix(pred.label,test.y)





# Example of Stacking algorithms
# create submodels
#install.packages("caretEnsemble")
#library(caretEnsemble)
#library(mlbench)
#library(caret)
#library(caretEnsemble)

#control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE)
#algorithmList <- c('rpart', 'glm','knn')
#set.seed(123)
#metric <- "Accuracy"
#models <- caretList(as.numeric(class)~., data=magicdata3, trControl=control,  methodList=algorithmList)
#preds<-predict(models,magicdata,type='prob')
#confusionMatrix(preds,magicdata$class)
#results <- resamples(models)
#summary(results)
#dotplot(results)


#staking 
predDF <- data.frame(preds_rf,pred.ano,pred.tan,preds.c5, class = test.data$class)
pred1V <- predict(model_rf, test.data)
pred2V  <- predict(dt_fit, test.data)
pred3v <- predict(model_svm.ano,test.data)
pred4v<-predict(model.tanhdot,test.data)
pred5v<-predict(c5_tree, test.data)
predVD <- data.frame(pred.rf = pred1V, pred.dt = pred2V,pred.svm.ano=pred3v,pred.tanh=pred4v,pred.c5=pred5v)
pred_test.svm <- predict(model_svm,test.data)
accuracy <- rbind(confusionMatrix(pred1V, test.data$class)$overall[1],confusionMatrix(pred2V, test.data$class)$overall[1],confusionMatrix(test.data$class,pred_test.svm)$overall[1],confusionMatrix(pred.ano,test.data$class)$overall[1],confusionMatrix(pred.tan,test.data$class)$overall[1],confusionMatrix(preds.c5, test.data$class)$overall[1])
row.names(accuracy) <- c("RF", "dt", "Stack","svmano","svmtan","c5")
accuracy 
dotplot(accuracy)
 