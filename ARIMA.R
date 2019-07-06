library("stats")
library("ggplot2")
library("tseries")
library("forecast")


#Setup
Rates<-read.csv("/Users/MattPettit/Documents/Senior/Spring 2019/Res 497/Variable 2.csv")
Rates.Q<-ts(Imports, frequency = 4)
View(Rates.Q)
plot(Rates.Q)

#Within Sample: Q1 1991-Q4 2013
Within.S<-ts(Rates.Q[1:60], frequency = 4,start = c(1999,1))
View(Rates.wi)

#Post Sample: Q1 2014-Q2 2018
Post.S<-ts(Rates.Q[61:78], frequency = 4, start = c(2014, 1))

#Dickey Fuller Test
adf.test(Within.S, alternative = "stationary")

#First Differencing 
Within.S_Diff<-diff(Within.S, differences = 1)
plot(Within.S_Diff)

#Dickey Fuller Test on 1st Diff
adf.test(Within.S_Diff, alternative = "stationary")

#Second Differencing **First Differencing did not pass DF test**
Within.S.2D<-diff(Within.S, differences = 2)
plot(Within.S.2D)

#Dickey Fuller Test on 2nd Diff
adf.test(Within.S.2D, alternative = "stationary")

#Determining Type of Process Via ACF/PACF
Acf(Within.S.2D, main = 'ACF for Within.S.2D')
Pacf(Within.S.2D, main = 'PACF for Within.S.2D')

#Automatic Selection
Auto.model<-auto.arima(Within.S, seasonal = TRUE)
print(Auto.model)


#ARIMA Forecasts
Rates.fcast<-forecast(Auto.model, h=18)
print(Rates.fcast)
Rates.fcast$series
print(Rates.fcast1)

#Post-sample Residuals
fcast.resid<-Rates.fcast$mean - Post.S
print(fcast.resid)
sum(fcast.resid)


#MSE
sqrt(mean(fcast.resid^2))

arima(Within.S, c(1,1,0))
Acf(Within.S_Diff, main = 'ACF for 1st Diff')
Pacf(Within.S_Diff, main = 'PACF For 1st Diff')

Rates<-read.csv("/Users/MattPettit/Documents/Senior/Spring 2019/Res 497/Variable 2.csv")
Rates.Q<-ts(Rates, frequency = 4)

#Within Sample
R.wi<-ts(Rates.Q[1:60], frequency = 4)
print(R.wi)

#Post Sample
R.p=ts(Rates.Q[61:78], frequency =4)
View(R.p)

#Developing naive no change--post sample forecasts
R.naive=ts(Rates.Q[60:77], frequency = 4)
View(R.naive)
Resid=R.naive - R.p
print(Resid)
sum(Resid)
sqrt(mean(Resid^2))

Theil.U=7.677/1.65
print(Theil.U)
