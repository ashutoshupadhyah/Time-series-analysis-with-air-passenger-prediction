install.packages('forecast') library(forecast) data("AirPassengers") class(AirPassengers) start(AirPassengers) end(AirPassengers) frequency(AirPassengers) sum(is.na(AirPassengers)) summary(AirPassengers) AirPassengers
tsdata<-ts(AirPassengers,frequency = 12) ddata<-decompose(tsdata,"multiplicative") plot(ddata)
plot(ddata$trend) plot(ddata$random) plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

cycle(AirPassengers)


boxplot(AirPassengers~cycle(AirPassengers, xlab="Date",ylab= "Passenger Numbers(1000's)"
                            
                            ,main="Monthly Air Passengers Boxplot from 1949 to 1961"))

boxplot(AirPassengers)

boxplot(AirPassengers,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961") 
mymodel<-auto.arima(AirPassengers)


mymodel

auto.arima(AirPassengers,ic="aic" ,trace = TRUE)

install.packages('tseries') library(tseries) adf.test(AirPassengers) plot.ts(mymodel$residuals)
acf(ts(mymodel$residuals),main='ACF Residual') pacf(ts(mymodel$residuals),main='PACF Residual') myforecast<-forecast(mymodel , level=c(95),h=10*12) plot(myforecast)
Box.test(mymodel$resid, lag=5, type = "Ljung-Box") Box.test(mymodel$resid, lag=10, type = "Ljung-Box") Box.test(mymodel$resid, lag=15, type = "Ljung-Box") 
