##input data
library(TSA)
library(tseries)
library(fGarch)
library(forecast)
sp500 = read.table("SP500 2.csv",header =T, sep =",")
##the daily values of NASDAQ Composite over the period from Jan 1st, 2005 to Jan 1st, 2015. 
spprice=sp500$Adj.Close ##take use of the adjusted close price
spreturn=diff(log(spprice)) ##logreturn
##plot
attach(mtcars)
par(mfrow=c(2,1))
plot(spprice,type="l",xlab="Time",ylab="NASDAQ Composite Price")
plot(spreturn,type="l",xlab="Time",ylab="NASDAQ Composite return")
##
attach(mtcars)
par(mfrow=c(3,2))
acf(spreturn,main="ACF NASDAQ log-return") ##autocorelation
pacf(spreturn,main="PACF NASDAQ log-return") ##partial acf
acf(abs(spreturn),main="ACF NASDAQ abs(log-return)")
pacf(abs(spreturn),main="PACF NASDAQ abs(log-return)")
acf(spreturn^2,main="ACF NASDAQ (log-return)^2")
pacf(spreturn^2,main="ACF NASDAQ (log-return)^2")
##display some significant autocorrelations and hence provide some evidence that the daily SP500 returns are not independently and identically distributed.
##suggest that the returns have much serial correlation at all
#extended acf (ESACF) for the time series 
eacf(spreturn) ##???  suggests that a white noise model is appropriate for these data. 

##model selection
arima010=arima(spreturn,order=c(0,1,0))
AIC(arima010)
arima110=arima(spreturn,order=c(1,1,0))
AIC(arima110)
arima011=arima(spreturn,order=c(0,1,1))
AIC(arima011)
arima111=arima(spreturn,order=c(1,1,1))
AIC(arima111)
arima012=arima(spreturn,order=c(0,1,2))
AIC(arima012)
arima210=arima(spreturn,order=c(2,1,0))
AIC(arima210)
arima112=arima(spreturn,order=c(1,1,2))
AIC(arima112)
arima211=arima(spreturn,order=c(2,1,1))
AIC(arima211)
arima212=arima(spreturn,order=c(2,1,2))
AIC(arima212)
arima020=arima(spreturn,order=c(0,2,0))
AIC(arima020)
arima120=arima(spreturn,order=c(1,2,0))
AIC(arima120)
arima021=arima(spreturn,order=c(0,2,1))
AIC(arima021)
arima121=arima(spreturn,order=c(1,2,1))
AIC(arima121)
arima022=arima(spreturn,order=c(0,2,2))
AIC(arima022)
arima220=arima(spreturn,order=c(2,2,0))
AIC(arima220)
arima122=arima(spreturn,order=c(1,2,2))
AIC(arima122)
arima221=arima(spreturn,order=c(2,2,1))
AIC(arima221)
arima222=arima(spreturn,order=c(2,2,2))
AIC(arima222)
##arima211 is selected!!!

arima211=arima(spreturn,order=c(2,1,1))
summary(arima211)
AIC(arima211)
BIC(arima211)
##
res.arima211=arima211$res
squared.res.arima211=res.arima211^2


##garch model selection
garch01=garch(x=res.arima211,order=c(0,1))
AIC(garch01)
garch02=garch(x=res.arima211,order=c(0,2))
AIC(garch02)
garch03=garch(x=res.arima211,order=c(0,3))
AIC(garch03)
garch04=garch(x=res.arima211,order=c(0,4))
AIC(garch04)
garch05=garch(x=res.arima211,order=c(0,5))
AIC(garch05)
garch06=garch(x=res.arima211,order=c(0,6))
AIC(garch06)
garch07=garch(x=res.arima211,order=c(0,7))
AIC(garch07)
garch08=garch(x=res.arima211,order=c(0,8))
AIC(garch08)
garch09=garch(x=res.arima211,order=c(0,9))
AIC(garch09)
garch10=garch(x=res.arima211,order=c(0,10))
AIC(garch10)
garch11=garch(x=res.arima211,order=c(1,1))
AIC(garch11)
garch12=garch(x=res.arima211,order=c(1,2))
AIC(garch12)
garch13=garch(x=res.arima211,order=c(1,3))
AIC(garch13)
###garch12 is selected!!!
##arima(2,1,1)+garch(1,2)

##log Likelihood
loglik.gar12=logLik(garch12)
loglik.ar211=logLik(arima211)
summary(garch12)
summary(loglik.ar211)

##conditional covariance
cv.garch12=garch12$fit[,1]^2
plot(cv.garch12)

##residuals analysis
##QQ-plot
par(mfcol=c(2,1))
qqnorm(res.arima211,main='ARIMA(2,1,1) Residuals');qqline(res.arima211)
argar.res=garch12$residuals
squared.res.argar=argar.res^2
qqnorm(argar.res,main='ARIMA(2,1,1)/GARCH(1,2) Residuals');qqline(argar.res)
par(mfcol=c(2,1))
##squared residuals plot
plot(squared.res.arima211,main='Squared Residuals',ylab="squared residuals ARIMA")
plot(squared.res.argar,main='S?quared Residuals',ylab="squared residuals ARIMA/GARCH")
##ACF PACF of residuals
par(mfcol=c(2,1))
acf(na.omit(squared.res.argar),main='ACF Squared Residuals ARIMA/GARCH')
pacf(na.omit(squared.res.argar),main='PACF Squared Residuals ARIMA/GARCH')
##armasubsets
armasubsets1=armasubsets(y=res.arima211,nar=14,nma=14,y.name='test',ar.method='ols')
armasubsets2=armasubsets(y=na.omit(argar.res),nar=14,nma=14,y.name='test',ar.method='ols')
par(mfcol=c(1,1))
#plot(armasubsets1)
plot(armasubsets2)

##Box-Ljung test
Box.test(x=residuals(arima211),lag=6,type = c("Box-Pierce", "Ljung-Box")[2])
Box.test(x=residuals(arima211),lag=12,type = c("Box-Pierce", "Ljung-Box")[2])
Box.test(x=residuals(arima211),lag=18,type = c("Box-Pierce", "Ljung-Box")[2])
##
Box.test(x=na.omit(argar.res),lag=6,type = c("Box-Pierce", "Ljung-Box")[2])
Box.test(x=na.omit(argar.res),lag=12,type = c("Box-Pierce", "Ljung-Box")[2])
Box.test(x=na.omit(argar.res),lag=18,type = c("Box-Pierce", "Ljung-Box")[2])

##input new data to compare with the prediction
sp500new=read.table("NASDAQ new.csv",header = T,sep=",")
spprice.new=sp500new$Adj.Close
Ts=length(spprice)
Tnew=length(spprice.new)
Tnew=length(spprice.new)
spprice.new2<-c()
for(i in 1:Tnew){
  spprice.new2[i+Ts]=spprice.new[i]
  }
##prediction!!!
##predict return
arima221pr=arima(log(spprice),order=c(2,2,1))
res.arima221pr=arima221pr$residuals
fitgar = garchFit(formula = ~ garch(1, 2), data = res.arima221pr, trace = FALSE)
predgar<-predict(fitgar, n.ahead=100, plot=T)
##predict price using ARIMA
predar<-predict(arima221pr, n.ahead=1000, plot=T)
pred2=predar$pred
low=pred2-1.96*predar$se 
high=pred2+1.96*predar$se  
par(mfcol=c(1,1))
plot(log(spprice),type='l',main='Prediction NASDAQ,Low,High',xlim=c(0,4000),ylim=c(7.0,10.0)) 
lines(pred2,col="green")
lines(low,col='red')
lines(high,col='blue')
points(log(spprice.new2),pch=17,col="grey")
##



##
##Computes the Augmented Dickey-Fuller test for the null that x has a unit root.
adf.test(na.omit(argar.res))
##Computes the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for the null hypothesis that x is level or trend stationary.
kpss.test(na.omit(argar.res))
##BDS Test
bds.test(na.omit(argar.res))
##Computes the Phillips-Perron test for the null hypothesis that x has a unit root.
pp.test(na.omit(argar.res))
##McLeod.Li.test
McLeod.Li.test(na.omit(argar.res))
##argar=garchFit(~arma(2,1) + garch(1, 1), data=as.ts(tail(spreturn,3000)))

##Generically computes the White neural network test for neglected nonlinearity either for the time series x or the regression y~x.