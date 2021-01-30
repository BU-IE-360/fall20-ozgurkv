library(readxl)
Consumption <- read_excel("C:/Users/ozgur/Desktop/Documents/IE 360/HW4/Consumption.xls")
View(Consumption)

tsdata <- ts(Consumption$consumption, start=c(2017, 0),frequency=24*365)
head(tsdata)
str(tsdata)
plot(tsdata)
acf(tsdata)
pacf(tsdata)

library(fpp)
library(forecast)
library(data.table)
cons <- dt$cons
cons
dt <- data.table("cons" = Consumption$consumption)


dt$cons
meancons <- c()
length <- length(cons)/24
i<-1
j<-1
while(i<length+1){
    meancons[i] <- mean(cons[c(j:(j+23))])
    j <- j+24
    i<-i+1
}
meancons

tsdata2 <- ts(meancons, start=c(2017,1,1),frequency=365.25)
ts.plot(tsdata2)

acf(tsdata2)
plot(tsdata2)

tsdata3 <- diff(tsdata2,7)
acf(tsdata3)

tsdata4 <- decompose(tsdata3,type="multiplicative")
deseasonalized <-tsdata3/tsdata4$seasonal
ts.plot(deseasonalized) #there is still trend
acf(deseasonalized)

detrend<-deseasonalized/tsdata4$trend
ts.plot(detrend)
acf(detrend, na.action = na.pass)
#no significant autocorrelation
random <- tsdata4$random
plot(tsdata4$random)



Box.test(tsdata2,lag=7,type="Ljung-Box")
#significant autocorrelation

Box.test(tsdata2,lag=365.25,type="Ljung-Box")
#significant autocorrelation

tsdecomp <- decompose(tsdata2,type="multiplicative")
plot(tsdecomp)

tsdecomp2 <- decompose(tsdata2,type="additive")
plot(tsdecomp2)

log(tsdata2)
plot(log(tsdata2))
plot(sqrt(tsdata2))

random <- tsdecomp$random
plot(random)

random2 <- tsdecomp2$random
plot(random2)

dtdeseasonal <- tsdata2/tsdecomp$seasonal
dtdetrend <- tsdata2/tsdecomp$trend
plot(dtdetrend)
acf(dtdetrend, na.action = na.pass)
pacf(dtdetrend, na.action = na.pass)

library(forecast)
library(stats)
auto.arima(random,seasonal=FALSE,trace=TRUE)
fit <- arima(dtdetrend, c(5,1,2))
fit
autoplot(forecast(fit,h=14))


