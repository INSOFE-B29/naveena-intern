rm(list=ls())
library(pastecs)
stat.desc(magicdata) 
library(readr)
magicdata <- read_csv("D:/intern dataset/magic04.data.txt", col_names = FALSE, trim_ws = FALSE)
colnames(magicdata)<-c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans","fAlpha","fDist","class")
sum(is.na(magicdata))
str(magicdata)
library(caret)

    
magicdata$class<-ifelse(magicdata$class=="g",1,0)
table(magicdata$class)
magicdata$class<-as.factor(magicdata$class)

library(corrplot)
corrplot(cor(magicdata[,-11]),method="number")
magicdata$fConc1<-NULL

#pca,preprocess
library(caret)
preprocessParams <- preProcess(magicdata[,-10], method=c("center", "scale","pca"))
print(preprocessParams)
magicdata.pca<-predict(preprocessParams,magicdata[,-10])
mydata<-cbind(magicdata.pca,magicdata[,10])

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
model_svm <- svm(class ~ . , train.data,kernel ="linear",type='C-classification',scale=FALSE,cost=1,gamma=0.1)
preds.model=predict(model_svm,test.data,type="class")
confusionMatrix(preds.model,test.data$class)

#logistic
log_reg <- glm(class~., data = train.data, family = binomial)
model_aic <- stepAIC(log_reg, direction = "both")
summary(model_aic)
summary(log_reg)
library(car)
vif(model_aic)
vif(log_reg)
prob_train <- predict(log_reg, type = "response")
library(ROCR)
pred <- prediction(prob_train, train.data$class)
perf <- performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))





 