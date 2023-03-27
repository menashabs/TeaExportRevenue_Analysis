###################################################
################################################### 
## Project - TeaExport

## Considering whole data set 

####Using data from 2007

# loading the libraries
library(forecast)
library(fUnitRoots)
library(lmtest)
library(readxl)

tea <- read_xlsx("TeaExportRevenue_Dataset.xlsx")

tea_data <- tea$tea_export_revenue
tea_data

DataFile_1 = ts(tea_data, frequency = 12, start = c(2008,1))
DataFile_1

Tea_Data <- DataFile_1
Tea_Data

length(Tea_Data)

trainingSet = window(ts(Tea_Data), 1, 140)
trainingSet

testingSet = window(ts(Tea_Data, 141, 163))
testingSet

trainingSet = ts(trainingSet, frequency = 12, start = c(2008,1))
trainingSet

testingSet = ts(testingSet, frequency = 12, start = c(2019,9))
testingSet

plot(trainingSet)

logTr = log(trainingSet)

plot(logTr)

Acf(logTr, 100)

Pacf(logTr, 100)

adfTest(logTr)

D12logTr = diff(logTr, 12)

plot(D12logTr)

Acf(D12logTr, 100)

Pacf(D12logTr, 100)

adfTest(D12logTr)

D12D1logTr = diff(D12logTr, 1)

plot(D12D1logTr)

Acf(D12D1logTr, 100)

Pacf(D12D1logTr, 100)

adfTest(D12D1logTr)

# model fitting 

fit1 = Arima(sqTr, order = c(2, 0, 0), seasonal = c(0, 1, 1))
fit1
summary(fit1)

coeftest(fit1)

fit2 = Arima(sqTr, order = c(0, 1, 1), seasonal = c(0, 1, 1))
fit2
summary(fit2)

coeftest(fit2)

Box.test(residuals(fit1), lag = 24, type = "Ljung")

Box.test(residuals(fit2), lag = 24, type = "Ljung")

shapiro.test(fit1)


# developing models further - all are not included here

fit3 = Arima(sqTr, order = c(2, 1, 0), seasonal = c(0, 1, 1))
fit3
summary(fit3)

coeftest(fit3)

Box.test(residuals(fit3), lag = 24, type = "Ljung")


fit4 = Arima(sqTr, order = c(2,0,1), seasonal = c(1, 1, 1))
fit4
summary(fit4)

coeftest(fit4)

Box.test(residuals(fit4), lag = 24, type = "Ljung")

fit5 = Arima(sqTr, order = c(2,1,1), seasonal = c(1, 1, 1))
fit5
summary(fit5)

coeftest(fit5)

Box.test(residuals(fit5), lag = 24, type = "Ljung")

fit6 = Arima(sqTr, order = c(1,1,1), seasonal = c(0, 1, 1))
fit6
summary(fit6)

coeftest(fit6)

Box.test(residuals(fit6), lag = 24, type = "Ljung")

fit7 = Arima(sqTr, order = c(0,1,1), seasonal = c(1, 1, 1))
fit7
summary(fit7)

coeftest(fit7)

Box.test(residuals(fit7), lag = 24, type = "Ljung")

fit8 = Arima(sqTr, order = c(1,0,1), seasonal = c(0, 1, 1))
fit8
summary(fit8)

coeftest(fit8)

Box.test(residuals(fit8), lag = 24, type = "Ljung")

fit8 = Arima(sqTr, order = c(1,0,1), seasonal = c(0, 1, 1))
fit8
summary(fit8)

coeftest(fit8)

Box.test(residuals(fit8), lag = 24, type = "Ljung")

fit9 = Arima(sqTr, order = c(1,0,1), seasonal = c(1,0, 1))
fit9
summary(fit9)

coeftest(fit9)

Box.test(residuals(fit9), lag = 24, type = "Ljung")

fit10 = Arima(sqTr, order = c(0,0,1), seasonal = c(1,1,0))
fit10
summary(fit10)

coeftest(fit10)

Box.test(residuals(fit10), lag = 24, type = "Ljung")


# forecasting 

ForeVal = forecast(fit2)
ForeVal

ForeVal_test = forecast(fit2, 35)
ForeVal_test

plot(forecast(fit2))


## BEFORE COVID-19

trainingSet_cov = window(ts(Tea_Data), 1, 126)
trainingSet_cov

testingSet_cov = window(ts(Tea_Data, 127, 157))
testingSet_cov

trainingSet_cov = ts(trainingSet_cov, frequency = 12, start = c(2007,1))
trainingSet_cov

testingSet_cov = ts(testingSet_cov, frequency = 12, start = c(2017,7))
testingSet_cov

plot(trainingSet_cov)

logTr_cov = log(trainingSet_cov)

plot(logTr_cov)

sqTr_cov <- sqrt(trainingSet_cov)

plot(sqTr_cov)

Acf(sqTr_cov, 100)

Pacf(sqTr_cov, 100)

adfTest(sqTr_cov)

D12sqTr_cov = diff(sqTr_cov, 12)

plot(D12sqTr_cov)

Acf(D12sqTr_cov, 100)

Pacf(D12sqTr_cov, 100)

adfTest(D12sqTr_cov)

D12D1sqTr_cov = diff(D12sqTr_cov, 1)

plot(D12D1sqTr_cov)

Acf(D12D1sqTr_cov, 100)

Pacf(D12D1sqTr_cov, 100)

adfTest(D12D1sqTr_cov)

# model fitting

fit1_cov = Arima(sqTr_cov, order = c(2, 0, 0), seasonal = c(0, 1, 1))
fit1_cov
summary(fit1_cov)

coeftest(fit1_cov)

fit2_cov = Arima(sqTr_cov, order = c(0, 1, 1), seasonal = c(0, 1, 1))
fit2_cov
summary(fit2_cov)

coeftest(fit2_cov)

Box.test(residuals(fit1_cov), lag = 24, type = "Ljung")

Box.test(residuals(fit2_cov), lag = 24, type = "Ljung")

# developing models further 

fit3_cov = Arima(sqTr_cov, order = c(2, 1, 0), seasonal = c(0, 1, 1))
fit3_cov
summary(fit3_cov)

coeftest(fit3_cov)

Box.test(residuals(fit3_cov), lag = 24, type = "Ljung")


fit4_cov = Arima(sqTr_cov, order = c(2,0,1), seasonal = c(1, 1, 1))
fit4_cov
summary(fit4_cov)

coeftest(fit4_cov)

Box.test(residuals(fit4_cov), lag = 24, type = "Ljung")

fit5_cov = Arima(sqTr_cov, order = c(2,1,1), seasonal = c(1, 1, 1))
fit5_cov
summary(fit5_cov)

coeftest(fit5_cov)

Box.test(residuals(fit5_cov), lag = 24, type = "Ljung")

fit6_cov = Arima(sqTr_cov, order = c(1,1,1), seasonal = c(0, 1, 1))
fit6_cov
summary(fit6_cov)

coeftest(fit6_cov)

Box.test(residuals(fit6_cov), lag = 24, type = "Ljung")

fit7_cov = Arima(sqTr_cov, order = c(0,1,1), seasonal = c(1, 1, 1))
fit7_cov
summary(fit7_cov)

coeftest(fit7_cov)

Box.test(residuals(fit7_cov), lag = 24, type = "Ljung")

fit8_cov = Arima(sqTr_cov, order = c(1,0,1), seasonal = c(0, 1, 1))
fit8_cov
summary(fit8_cov)

coeftest(fit8_cov)

Box.test(residuals(fit8_cov), lag = 24, type = "Ljung")


fit9_cov = Arima(sqTr_cov, order = c(1,0,1), seasonal = c(1,0, 1))
fit9_cov
summary(fit9_cov)

coeftest(fit9_cov)

Box.test(residuals(fit9_cov), lag = 24, type = "Ljung")

fit10_cov = Arima(sqTr_cov, order = c(0,0,1), seasonal = c(1,1,0))
fit10_cov
summary(fit10_cov)

coeftest(fit10_cov)

Box.test(residuals(fit10_cov), lag = 24, type = "Ljung")

# forecasting 

ForeVal = forecast(fit2_cov)
ForeVal

ForeVal_test = forecast(fit2_cov, 31)
ForeVal_test

plot(forecast(fit2_cov))





