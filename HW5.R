library(tseries)
library(forecast)
library(stats)
library(magrittr)
#5.2a
data52<-c(29,20,25,29,31,33,34,27,26,30,29,28,28,26,27,26,30,28,26,30,31,30,37,30,33,31,27,33,37,29,28,30,29,34,30,20,17,23,24,34,36,35,33,29,25,27,30,29,28,32)
plot(data52,type="b",pch=19,main="TS Plot")
#5.2b
acf(data52,lag.max=20,type="correlation",main="ACF for y")
acf(data52, lag.max=20,type="partial",main="PACF for y")
#5.2c
auto.arima(data52,seasonal = FALSE,test = "adf",ic = "aic")
data52.fit.ar<-arima(data52,order=c(0,0,1))
data52.fit.ar
res.data52.ar<-as.vector(residuals(data52.fit.ar))
fit.data52.ar<-as.vector(fitted(data52.fit.ar))
Box.test(res.data52.ar,lag=48,fitdf=2,type="Ljung")
par(mfrow = c(1,2),oma=c(0,0,0,0))
acf(res.data52.ar,lag.max=25,type="correlation",main="ACF of the
    Residuals \nof MA(1) Model")
acf(res.data52.ar, lag.max=25,type="partial",main="PACF of the
    Residuals \nof MA(1) Model")
par(mfrow = c(1,1))
#5.2d
onestep52 = sapply(40:49,function(x){
  as.vector(forecast(arima(data52,order=c(0,0,1)),h=1)$mean)
})
names(onestep52) = 41:50
fitted52=as.vector(fitted(fit52))[41:50]
onestep52
sse52= sum((onestep52- fitted52)^2)
sse52
#5.11
data511<-read.csv("~/Desktop/data511.csv",header = FALSE)
names(data511) = c("week","sales")
plot(1:110,data511[1:110,2],type="b",pch=19,main="TS Plot")
par(mfrow = c(1,2),oma=c(0,0,0,0))
acf(data511[1:110,2],lag=28,type= "correlation",main="ACF")
acf(data511[1:110,2],lag=28,type="partial",main="PACF")
par(mfrow = c(1,1))
fit511 = arima(data511[1:110,2],c(1,0,1))
par(mfrow = c(1,2),oma=c(0,0,0,0))
acf(fit511$residuals,lag=25,"correlation",main="ACF of the
    Residuals ")
acf(fit511$residuals,lag=25,type="partial",main="PACF of the
    Residuals")
par(mfrow = c(2,2))
qqnorm(as.vector(fit511$residuals) ,datax = T,pch=19)
qqline(fit511$residuals,datax = T)
plot(as.vector(fitted(fit511)),fit511$residuals,main="fitted value",pch=19)
abline(h=0);
hist(as.vector(fit511$residuals),col="grey",main="residuals")
par(mfrow = c(1,1))
forecast(fit511,h=10)
#5.26
data526<-read.csv("~/Desktop/data526.csv",header = FALSE)
data526.fit.ar<-arima(data526[1:84,2],order=c(1,1,1))
data526.fit.ar
res.data526.ar<-as.vector(residuals(data526.fit.ar))
fit.data526.ar<-as.vector(fitted(data526.fit.ar))
Box.test(res.data526.ar,lag=48,fitdf=2,type="Ljung")
par(mfrow = c(1,2),oma=c(0,0,0,0))
acf(res.data526.ar,lag.max=25,type="correlation",main="ACF of the
    Residuals ")
acf(res.data526.ar, lag.max=25,type="partial",main="PACF of the
    Residuals ")
par(mfrow = c(1,1))
onestep526 = sapply(83:95,function(x){
  as.vector(forecast(arima(data526[,2],order=c(1,1,1)),h=1)$mean)
})
names(onestep526) = 84:96
data526.fit= arima(data526[,2],order=c(1,1,1))
fitted526=as.vector(fitted(data526.fit))[84:96]
sse526= sum((onestep526- fitted526)^2)
onestep526
sse526

