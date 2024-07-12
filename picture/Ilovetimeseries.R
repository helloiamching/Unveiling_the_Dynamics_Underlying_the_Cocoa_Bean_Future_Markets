library(pracma)
library(lubridate)
library(seastests)
library(forecast)
library(sarima)
library(seastests)
library(tseries)
library(astsa)
library(TSA)

cocoa = read.csv('/Users/wang.c/Downloads/CC=F1.csv')


n = length(cocoa$Open)


cocoa = cocoa[-((n-4):n),]


my.ts = sqrt(cocoa$Volume)
adf.test(my.ts)


ts.plot(my.ts)
Acf(my.ts,lag = 100)
Pacf(my.ts,lag= 100)



seastests::fried(diff(my.ts),13)
seastests::kw(diff(my.ts),13)
seastests::qs(diff(my.ts),13)
seastests::combined_test(diff(my.ts),4)



dif.13 = diff(my.ts,lag =13)
ts.plot(dif.13)
Acf(dif.13,lag = 100)
Pacf(dif.13,lag = 100)

astsa::sarima(dif.13,S=13,p=9,d=1,q=8,P=1,D=0,Q=0)

auto = auto.arima(my.ts)
checkresiduals(auto$residuals)

per = periodogram(dif.13)
require(data.table)
data.table(period = 1/per$frequency,spec = per$spectrum)[order(-spec)]
