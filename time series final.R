rm(list=ls())
setwd("D:\\phd\\m.l")
org.data<-read.csv("Train.csv")
sum(is.na(org.data))
#wc<-subset(org.data, select=c("MenClothing"))
wc <- subset(org.data, ProductCategory=="WomenClothing") 
sum(is.na(wc))
library(DMwR)
wc<-centralImputation(wc)
mc <- subset(org.data, ProductCategory=="MenClothing") 
sum(is.na(mc))
mc<-centralImputation(mc)
oc <- subset(org.data, ProductCategory=="OtherClothing") 
sum(is.na(oc))
oc<-centralImputation(oc)

table(wc$sales)

library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
library(STARTS)
summary(wc)
#plt.plot(wc)
colnames(wc)[4] <- "sales"
colnames(mc)[4] <- "sales"
colnames(oc)[4] <- "sales"

###### WCconverting into time series#####
wcts <- ts(wc$sales, frequency =12)
#######Decomposed Time Series######
#Decompose will provide us with the info on seasonality,trend and randomness
wcdecompose=decompose(wcts)
plot(wcdecompose,col="blue",xlab="Year")

###### mc converting into time series#####
mcts <- ts(mc$sales, frequency =12)
#######Decomposed Time Series######
#Decompose will provide us with the info on seasonality,trend and randomness
mcdecompose=decompose(mcts)
plot(mcdecompose,col="Red",xlab="Year")

###### oc converting into time series#####
octs <- ts(oc$sales, frequency =12)
#######Decomposed Time Series######
#Decompose will provide us with the info on seasonality,trend and randomness
ocdecompose=decompose(octs)
plot(ocdecompose,col="yellow",xlab="Year")


####plots###
wsts <- ts(wc$sales, frequency =12)
plot(wsts,type="h",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="Time series plot for Women Category with
     Imputing")
mcts <- ts(mc$sales, frequency =12)
plot(mcts,type="h",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="Time series plot for Women Category with
     Imputing")
octs <- ts(mc$sales, frequency =12)
plot(octs,type="h",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="Time series plot for Women Category with
     Imputing")

### sesonality#####
wsts <- ts(wc$sales, frequency =12)
plot(wsts,type="l",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="seasonal plot for Women Category")

msts <- ts(mc$sales, frequency =12)
plot(msts,type="l",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="seasonal plot for men Category")

osts <- ts(oc$sales, frequency =12)
plot(osts,type="l",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="seasonal plot for other Category")


orgts <- ts(org.data$Sales.In.ThousandDollars., frequency =12)
plot(orgts,type="h",lwd=2,col="red",xlab="year(In all months)",ylab="Slaes Amount in Thousand Dollars", main="Time series plot for Women,men,other without Imputing")

#auto arima #wc  #17.94018%
MODEL_ARIMA <- auto.arima(wc$sales, ic='aic')
summary(MODEL_ARIMA)
class(wc$sales)
pricearimaforecasts1 <- forecast(MODEL_ARIMA, h=12)
plot(pricearimaforecasts1)
a<-data.frame(pricearimaforecasts1$mean)
write.csv(a,file = "wom.csv")

#mc

MODEL_ARIMA1 <- auto.arima(mc$sales, ic='aic')
summary(MODEL_ARIMA1)
#class(wc$sales)
pricearimaforecasts2 <- forecast(MODEL_ARIMA1, h=12)
plot(pricearimaforecasts2)
b<-data.frame(pricearimaforecasts2$mean)
write.csv(b,file = "men.csv")
#oc
MODEL_ARIMA2 <- auto.arima(oc$sales, ic='aic')
summary(MODEL_ARIMA2)
#class(wc$sales)
pricearimaforecasts3 <- forecast(MODEL_ARIMA2, h=12)
plot(pricearimaforecasts3)
c<-data.frame(pricearimaforecasts3$mean)
write.csv(c,file = "oth.csv")


#######holtwinter wc#######
#par(mfrow=c(2,2))
wc.ts <- ts(wc$sales, start = c(2009,1),   frequency = 12)
wc.hw <- HoltWinters(wc.ts,gamma=TRUE,seasonal = c("additive"),optim.start = c(alpha =0.3, beta =  0.1, gamma =  0.1),optim.control = list())
plot(wc.hw, col = "blue", col.predicted = "red")
holtsaleforecast<-  forecast(wc.hw,h = 12)
a1<-data.frame(holtsaleforecast$mean)
write.csv(a1,file = "wom1.csv")

#######holtwinter mc#######
mc.ts <- ts(mc$sales, start = c(2009,1),    frequency = 12)
mc.hw <- HoltWinters(mc.ts,seasonal = c("multiplicative"),gamma=FALSE,optim.start = c(alpha =0.3, beta =  0.1, gamma =  0.1),optim.control = list())
plot(mc.hw, col = "blue", col.predicted = "red")
holtsaleforecast1<-  forecast(mc.hw,h = 12)
b1<-data.frame(holtsaleforecast1$mean)
write.csv(b1,file = "men1.csv")

#######holtwinter oc######
oc.ts <- ts(oc$sales, start = c(2009,1),    frequency = 12)
oc.hw <- HoltWinters(oc.ts,seasonal = c("multiplicative"),gamma=FALSE,optim.start = c(alpha =0.3, beta =  0.1, gamma =  0.1),optim.control = list())
plot(oc.hw, col = "blue", col.predicted = "red")
holtpriceforecast2<-  forecast(oc.hw,h = 12)
c1<-data.frame(holtpriceforecast2$mean)

write.csv(c1,file = "oth1.csv")

####acf,pacf for womens clothing ###
#par(mfrow=c(2,2))
acf(wc$sales,lag=30)
pacf(wc$sales,lag=30)

####acf,pacf for men clothing ###
acf(mc$sales,lag=30)
pacf(mc$sales,lag=30)

####acf,pacf for men clothing ###
acf(oc$sales,lag=30)
pacf(oc$sales,lag=30)

#11.93397% ema
####exponential moving average on women clothing####
fitEma1 <- EMA(wcts, n = 2)
predema1 <- forecast(fitEma1[!is.na(fitEma1)],h=12)
plot(predema1)
emapred<-  forecast(fitEma1,h = 12)
emao.p<-data.frame(emapred$mean)
write.csv(emao.p,file = "ema.wc.csv")

####exponential moving average on men clothing####
fitEma1 <- EMA(mcts, n = 2)
predema1 <- forecast(fitEma1[!is.na(fitEma1)],h=12)
plot(predema1)
emapred1<-  forecast(fitEma1,h = 12)
emao.p1<-data.frame(emapred1$mean)
write.csv(emao.p1,file = "ema.mc.csv")

####exponential moving average on other clothing####
fitEma2 <- EMA(osts, n = 2)
predema2 <- forecast(fitEma2[!is.na(fitEma2)],h=12)
plot(predema2)
emapred2<-  forecast(fitEma2,h = 12)
emao.p2<-data.frame(emapred2$mean)
write.csv(emao.p2,file = "ema.oc.csv")
