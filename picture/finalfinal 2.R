#####################################################################
####################### Loading Packages#############################
#####################################################################
library(seastests)
library(forecast)
library(TSA)
library(sarima)
#####################################################################
####################### Loading Data#################################
#####################################################################

cocoa = read.csv('/Users/wuguanxun/Downloads/CC=F1.csv')
cocoa = cocoa$Volume


n = length(cocoa)
cocoa.test =  cocoa[((n-51):n)]
cocoa = cocoa[-((n-51):n)]

#####################################################################
#######################Prelimary Analysis############################
#####################################################################

ts.plot(cocoa)

my.ts = log(cocoa+1)

ts.plot(my.ts)
acf(my.ts,lag = 1000, main = "")
pacf(my.ts,lag = 1000, main = "")

#####################################################################
#######################Stationary Test###############################
#####################################################################

adf.test(my.ts)

ocsb(my.ts,method = 'OLS',freq = 52)$Pval


#####################################################################
#######################Model Fitting#################################
#####################################################################


model1 = stats::arima(my.ts,order = c(13,0,1),method = "CSS-ML")
model2 = stats::arima(my.ts,order = c(26,0,1),method = "CSS-ML")
model3 = stats::arima(my.ts,order = c(1,0,13),method = "CSS-ML")
model4 = stats::arima(my.ts,order = c(1,0,26),method = "CSS-ML")

model1.refit = TSA::arima.boot(model1)

model1 = sarima(my.ts,p=13,d=0,q=1)
model2 = sarima(my.ts,p=26,d=0,q=1)
model3 = sarima(my.ts,p=1,d=0,q=13)
model4 = sarima(my.ts,p=1,d=0,q=26)

model5 = astsa::sarima(my.ts,p=13,d=0,q=1,P=1,D=0,Q=0,S=52)

model6 = astsa::sarima(my.ts,p=13,d=0,q=1,P=0,D=0,Q=1,S=52)

model7 = astsa::sarima(my.ts,p=13,d=0,q=1,P=1,D=0,Q=1,S=52)

model8 = astsa::sarima(my.ts,p=13,d=0,q=0,P=1,D=0,Q=0,S=52)

model9 = astsa::sarima(my.ts,p=13,d=0,q=0,P=0,D=0,Q=1,S=52)

model10 = astsa::sarima(my.ts,p=13,d=0,q=0,P=1,D=0,Q=1,S=52)

model11 = astsa::sarima(my.ts,p=26,d=0,q=0,P=0,D=0,Q=1,S=52)

model12 = astsa::sarima(my.ts,p=26,d=0,q=1,P=0,D=0,Q=1,S=52)

model13 = astsa::sarima(my.ts,p=26,d=0,q=1,P=1,D=0,Q=0,S=52)


test1 = model11$ttable
test1[,4]<0.05

checkresiduals(model10)
Box.Ljung.Test(model7$fit$residuals,lag = 50)

#####################################################################
#######################Ljung-Box Test################################
#####################################################################


#diagnosis sarima_model
residuals = model7$fit$residuals
par(mfrow=c(2,2))
ts.plot(residuals, main="Residuals", ylab="Residuals")
acf(residuals, main="ACF of Residuals", lag = 50)
qqnorm(residuals)
qqline(residuals, col="red")

lb_test_results <- sapply(0:100, function(lag) {
  Box.test(residuals, lag=lag, type="Ljung-Box")$p.value
})

# Plot Ljung-Box test p-values
plot(0:100, lb_test_results, type="b", pch=19, xlab="Lag", ylab="P-value",
     main="Ljung-Box Test P-values", ylim = c(0,1))
abline(h=0.05, col="red", lty=2)  # Add a horizontal line at 0.05 for significance level




find.sig.lags = function(residu){
  p.v = 1
  df= 0
  while(p.v>0.05){
    df = df+1
    p.v = Box.test(residu,type = "Ljung-Box",lag = df)$p.value
  }
  return(df)
}

find.sig.lags(resid(model1),fitdf = 14)
find.sig.lags(resid(model2),fitdf = 27)
find.sig.lags(resid(model3),fitdf = 14)
find.sig.lags(resid(model4),fitdf = 27)

find.sig.lags(resid(model5$fit))
find.sig.lags(resid(model6$fit))
find.sig.lags(resid(model7$fit))
find.sig.lags(resid(model8$fit))
find.sig.lags(resid(model9$fit))
find.sig.lags(resid(model10$fit))
find.sig.lags(resid(model11$fit))
find.sig.lags(resid(model12$fit))
find.sig.lags(resid(model13$fit))



AIC(model5$fit,model7$fit,model8$fit,model10$fit,model13$fit)
AIC(model10$fit)
AIC(model13$fit)


BIC(model7$fit$loglik)
BIC(model10$fit)
BIC(model13$fit)

