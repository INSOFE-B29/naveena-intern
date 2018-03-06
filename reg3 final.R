ibrary(dummies)
rm(list = ls())
data <- read.csv("train.csv")
library(xlsx)
#reading wc data
org.data<-read.csv("Train.csv")
wc <- subset(org.data, ProductCategory=="WomenClothing") 
sum(is.na(wc))
str(wc)
colnames(wc)[4] <- "sales"
wc<-centralImputation(wc)



######Reading holiday,macrd.org,weather data#######
macrd.orgdata <- read.xlsx("MacroEconomicData.xlsx",sheetName = "Sheet1")
require(gdata)
weatherdata2009 <- read.xlsx("WeatherData.xlsx", sheetName = "2009")
weatherdata2010 <- read.xlsx("WeatherData.xlsx", sheetName = "2010")
weatherdata2011 <- read.xlsx("WeatherData.xlsx", sheetName = "2011")
weatherdata2012 <- read.xlsx("WeatherData.xlsx", sheetName = "2012")
weatherdata2013 <- read.xlsx("WeatherData.xlsx", sheetName = "2013")
weatherdata2014 <- read.xlsx("WeatherData.xlsx", sheetName = "2014")
weatherdata2015 <- read.xlsx("WeatherData.xlsx", sheetName = "2015")
weatherdata2016 <- read.xlsx("WeatherData.xlsx", sheetName = "2016")
weatherdata <- rbind(weatherdata2009,weatherdata2010,weatherdata2011,weatherdata2012,weatherdata2013,weatherdata2014,weatherdata2015,weatherdata2016)
holiday<- read.xlsx("Events_HolidaysData.xlsx",sheetName = "Sheet3")


#######macrd.org preprocess#######

macr.month <- substr(macrd.orgdata$Year.Month,8,10)
macr.month <- as.data.frame(macr.month)
macrd <- cbind(macrd.orgdata,macr.month)
macrd_mon<-subset(macrd,select=c(19))

macr.year <- substr(macrd.orgdata$Year.Month,1,4)
macr.year <- as.data.frame(macr.year)
macrd.org <- cbind(macrd_mon,macr.year)
macrd.org <-cbind(macrd.org,macrd.orgdata)
macrd.org $Year.Month <- NULL

#changing col names
colnames(macrd.org)[1] <- "year"
colnames(macrd.org)[2] <- "month"
macrd.orgdata
sum(is.na(macrd.orgdata))

#weather preprocess
sapply(weatherdata,class)
weatherdata2010 $Year <- 2010
weatherdata2011 $Year <- 2011
weatherdata2012 $Year <- 2012
weatherdata2013 $Year <- 2013
weatherdata2014 $Year <- 2014
weatherdata2015 $Year <- 2015
weatherdata2016 $Year <- 2016
weatherdata <- rbind(weatherdata2009,weatherdata2010,weatherdata2011,weatherdata2012,weatherdata2013,weatherdata2014,weatherdata2015,weatherdata2016)
sum(is.na(weatherdata))
#weatherdata<-centralImputation(weatherdata)
sum(is.na(weatherdata$Month))
weatherdata <- subset(weatherdata, !is.na(weatherdata$Month))
sapply(weatherdata, function(weatherdata) sum(is.na(weatherdata)))
weatherdata$WeatherEvent<- NULL
weatherdata$Precip.Â..mm..sum<- NULL
weatherdata[weatherdata == '-'] = "NA"
weatherdata <- centralImputation(weatherdata)
sum(is.na(weatherdata))
str(weatherdata)


####### converting characters to numeric########
hn <- sapply(weatherdata[,-c(1:2)], function(weatherdata) as.numeric(as.character(weatherdata)))
str(hn)
hn<-as.data.frame(hn)
weatherdata<-cbind(hn,weatherdata[,c(1:2)])
str(weatherdata)
sum(is.na(weatherdata))
weatherdata<-centralImputation(weatherdata)





###########convert day to month############

weatherdata.org <- weatherdata %>% group_by(Year,Month) %>% summarise("Temp high (°C)" = mean(Temp.high..Â.C.),
                                                                      "Temp avg (°C)" = mean(Temp.avg..Â.C.),
                                                                      "Temp low (°C)" = mean(Temp.low..Â.C.),
                                                                      "Dew Point high (°C)" = mean(Dew.Point.high..Â.C.),
                                                                      "Dew Point avg (°C)" = mean(Dew.Point.avg..Â.C.),
                                                                      "Dew Point low (°C)" = mean(Dew.Point.low..Â.C.),
                                                                      "Humidity (%) high" = mean(HumidityÂ.....high),
                                                                      "Humidity (%) avg" = mean(HumidityÂ.....avg),
                                                                      "Humidity (%) low" = mean(HumidityÂ.....low),
                                                                      "Sea Level Press (hPa) high" = mean(Sea.Level.Press.Â..hPa..high),
                                                                      "Sea Level Press (hPa) avg" = mean(Sea.Level.Press.Â..hPa..avg),
                                                                      "Sea Level Press (hPa) low" = mean(Sea.Level.Press.Â..hPa..low),
                                                                      "Visibility (km) high" = mean(VisibilityÂ..km..high),
                                                                      "Visibility (km) avg" = mean(VisibilityÂ..km..avg),
                                                                      "Visibility (km) low" = mean(VisibilityÂ..km..low),
                                                                      "Wind (km/h) high" = mean(WindÂ..km.h..high),
                                                                      "Wind (km/h) avg" = mean(WindÂ..km.h..avg),
                                                                      "Wind (km/h) low" = mean(WindÂ..km.h..low))

#holiday
hldy = holiday
hldy$X<-NULL
hldy$Year<-NULL
hldy$Total<-NULL
sum(is.na(hldy))

########strctures########
str(hldy)
str(macrd.org)
macrd.org$AdvertisingExpenses..in.Thousand.Dollars.<-NULL
str(weatherdata.org)
weatherdata.org$X<-NULL
weatherdata.org$Year<-NULL
weatherdata.org$Month<-NULL
#str(macrd.org)
macrd.org$year<-NULL
macrd.org$Year<-NULL
macrd.org$month<-NULL
macrd.org$PartyInPower<-NULL
str(macrd.org)
str(weatherdata.org)

###### boxplot####
boxplot(hldy)
boxplot(hldy$Federal.Holiday)
boxplot(macrd.org)
boxplot(weatherdata.org)
boxplot(train.data)
boxplot(test.data)

# Combining three data frames #####
mydata<- cbind(hldy, weatherdata.org,macrd.org)
str(mydata)
mydata1 <- list(hldy, weatherdata.org,macrd.org)
mydata <- as.data.frame(mydata1)
str(mydata)
colnames(mydata)
mydata2 <- mydata[c(1:84),c(1:26)]
train.data<-cbind(mydata2,wc[,"sales"])
str(train.data)
colnames(train.data)[27] <- "TARGET"
test.data <- mydata[c(85:96),c(1:26)]
library(caret)
std_method <- preProcess(train.data[,-c(27)],method = c("center", "scale"))
#remove(std_method)
train.data <- predict(std_method,train.data)
test.data <- predict(std_method,test.data)
str(test.data)
train.data$AdvertisingExpenses..in.Thousand.Dollars.<-NULL
str(train.data)
train.data$X<-NULL
test.data$X<-NULL
test.data$AdvertisingExpenses..in.Thousand.Dollars.<-NULL
sapply(test.data, class)
#converting factor to dummy
train.data1<- dummy.data.frame(train.data, sep = ".")
test.data1<- dummy.data.frame(test.data, sep = ".")
str(train.data1)
str(test.data1)

#######model building on rf######## #11.95964%
library(randomForest)
model_rf <- randomForest(TARGET ~ . , train.data)
importance(model_rf)
varImpPlot(model_rf)
predsrf <- predict(model_rf,train.data)
err <- regr.eval(predsrf,train.data$TARGET)
predsrf <- predict(model_rf,test.data)
write.csv(predsrf,file = "pred.csv")

###rf with imp variables### #12.01717%
model_rf.iv <- randomForest(TARGET ~ Monthly.Nominal.GDP.Index..inMillion..+Monthly.Real.GDP.Index..inMillion..+CPI+unemployment.rate+Month+Federal.Holiday+Event, data = train.data)
predsrf.iv <- predict(model_rf.iv ,test.data)
write.csv(predsrf.iv,file = "rf.iv.csv")

##### rf with tuning####### #10.77889%
control <- trainControl(method="repeatedcv", number=10, repeats=1, search="random")
set.seed(123)
mtry <- sqrt(ncol(test.data))
rf_random <- train(TARGET~., data=train.data, method="rf", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
preds.rf.cv<-predict(rf_random,test.data)
write.csv(preds.rf.cv,file = "rf.tune.csv")
str(train.data)

########model building on xgboost######## 44.4667%
library(vegan)
library(dummies)
library(xgboost)
xgmodel = xgboost(data = as.matrix(train.data1), label = as.numeric(train.data1$TARGET), nrounds = 200)
str(train.data1)
#testing on train data 
y_pred = predict(xgmodel, newdata = as.matrix(test.data1))
#table (test.data1,ifelse(y_pred >= 1.5,1,0))
predsboost<-predict(xgmodel,as.matrix(test.data1))
write.csv(y_pred,file = "boost.csv")


##### Gradient Boosting model #########  #137.73  #7.260587%
library(gbm)

modelgbm <- gbm(TARGET ~ Monthly.Nominal.GDP.Index..inMillion..+Monthly.Real.GDP.Index..inMillion..+CPI+unemployment.rate+Month+Federal.Holiday+Event,data = train.data,n.trees = 1600)

#predtraingbm <- predict(modelgbm,n.trees = 1600)
#predgbm <- predict(modelgbm,Train,n.trees = 1600)
#err <- regr.eval(predgbm,Train$Sales.In.ThousandDollars.)
pred.gbm<- predict(modelgbm,test.data,n.trees = 1600)
write.csv(pred.gbm,file = "gbm.csv")

####linear regression### # 14.93414%
library(MASS)
library(car)
modelreg <- lm(TARGET~.,train.data)
summary(modelreg)
stepAIC(modelreg)
preds.lm<-predict(modelreg,test.data)
write.csv(preds.lm,file = "lm.csv")

predTest <- predict(modelreg,Test)
write.csv(predTest,file = "predTest22.csv")

