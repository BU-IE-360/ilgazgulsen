install.packages("readr")
install.packages("forecast")
install.packages("xlsx")
install.package("zoo")
install.packages("xts")
install.packages("data.table")
install.packages("normwhn.test")
install.packages("na.tools")
install.packages("stats")
library(zoo)
library(readr)
library(forecast)
library(readxl)
library(xts)
library(data.table)
library(normwhn.test)
library(na.tools)
library(stats)

datason<-data.table(read_excel("C:/Users/LENOVO/Desktop/son.xlsx",n_max=37850)) #Data read from Excell file
datason$Tarih<-as.character(datason$Tarih) #Ýn order to easy to work it convert to 'Tarih' to character
datason[,c("year","month","day"):=tstrsplit(Tarih,"-")] #Add 3 more column which shows year,month and day information seperately
str(datason)
is.ts(datason)
acf(datason)
datason$day=as.integer(datason$day) #Convert day,month,year to integer
datason$month=as.integer(datason$month)
datason$year=as.integer(datason$year)


cons_data <- datason[1:37848,3] #Select the consumption part to easier work



is.ts(cons_data) #check whether consumption is time series or not 
acf(cons_data) #brief look at the consumption 
datason[,"weekday" := format(as.Date(datason$Tarih,format = "%Y-%d-%m"),format = "%A")] #add also the weekday column

daily<-datason[,list(dailysum=sum(Tüketim)), by = list(year,month,day)] #Aggregate  hourly data to daily,monthly and yearly
monthly<-datason[,list(monthlysum=sum(Tüketim)), by = list(year,month)]
weekday<-datason[,list(weekdaysum=sum(Tüketim)),by=list(year,month,weekday)]
yearly<-datason[,list(yearlysum=sum(Tüketim)), by = list(year)]
d <-daily[,4] #select only the consumption column
acf(d)
plot(d)
acf(monthly)
acf(weekly)
m<-monthly[,3] #select only the consumption column
y<-yearly[,2] #select only the consumption column
datason <- as.xts(datason$Tüketim,order.by=as.Date(datason$Tarih))
weekly2 <- apply.weekly(datason,sum) #aggregate hourly data to weekly data 
acf(daily)
acf(monthly)
acf(yearly)
acf(weekly2)

weekday<-na.rm(weekday)

#creating time series objects  
ts_hourly<-ts(cons_data,freq=8760,start=c(2016,1,1)) #  (365*24)
ts_daily<-ts(d,freq=365,start=c(2016,1,1))
ts_monthly<-ts(m,freq=12,start=c(2016,1))
ts_yearly<-ts(y,freq=1,c(2016,1))
ts_weekly<-ts(weekly2,freq=52,start=c(2016,1))

acf(ts_hourly,lag.max=200) #acf of time series objects
acf(ts_daily,lag.max=400)
acf(ts_monthly,lag.max=200)
acf(ts_weekly,lag.max=200)



ts.plot(ts_daily,xlab = "Year",  ylab = "Consumption", main = "Daily")  #plots of time series objects
ts.plot(ts_monthly,xlab = "Year",  ylab = "Consumption", main = "Monthly")
ts.plot(ts_weekly,xlab = "Year", ylab = "Consumption", main = "Weekly")
ts.plot(ts_hourly,xlab = "Year",  ylab = "Consumption", main = "Hourly")
ts.plot(ts_yearly,xlab = "Year",  ylab = "Consumption", main ="Yearly" )


lh<-log(ts_hourly) #stabilize variance by log transformation
ld<-log(ts_daily)
lm<-log(ts_monthly)
lw<-log(ts_weekly)

ts.plot(ts_daily)
ts.plot(ld)


dhourly <-decompose(lh,type="additive") #decompose of time series objects in additive way 
ddaily <-decompose(ld,type="additive")
dmonthly <-decompose(lm,type="additive")
dweekly <-decompose(ts_weekly,type="additive" ) 



plot(dhourly)
plot(ddaily)
plot(dmonthly)
plot(dweekly)



seasonplot(ts_hourly)
seasonplot(ts_daily)



dechour<-decompose(ts(ts_hourly, frequency = 168)) #since assumption is day+hour seasonality decompose the data 
plot(dechour)
A[3000:37847]$trend

a<-ts(ts_hourly,frequency = 168) #Set the frequency equal 168
acf(a,lag.max = 400) #acf 
plot(a) #plot 


deseasonal<-a-dechour$seasonal  #subtract the seasonal component
ts.plot(deseasonal,main='deseasonal') #plot without seasonal component
acf(deseasonal,main='deseasonal') #acf without seasona component


detrend<-deseasonal-dechour$trend #also subtract the trend
acf(detrend, na.action=na.pass,main='deseasonal with detrend1') #acf without both seasonal and trend component
plot(detrend,main='deseasonal with detrend') #plot without both seasonal and trend component


arm <- arima(detrend, order=c(1,0,0)) #autoregressive model
print(arm)
AIC(arm)
arm2<- arima(detrend, order=c(2,0,0)) #autoregressive model2 with different p (will be selected)
print(arm2)
AIC(arm2) #

#part4
mam <- arima(detrend, order=c(0,0,1)) # moving average model
print(mam)
AIC(mam)
mam2 <- arima(detrend, order=c(0,0,2)) #moving average model2 with different q
print(mam2)
AIC(mam2)

model_fitted <- a - residuals(ar2)
points(model_fitted, type = "l", col = 2, lty = 2)
#part5
model_forecast <- predict(arm2, n.ahead = 24)$pred #prediction for 24 period 

model_forecast_se <- predict(arm2, n.ahead = 24)$se #error 
 
seasonal<-dechour$seasonal #obtain seasonality 
sez<-seasonal [1:24]

newtrend<-dechour$trend #obtain trend
newt<-newtrend[37764] #trend of last observation
trend_vector<-rep(newt,times=24)

print(newt)
print(sez)
print(model_forecast)
print(trend_vector)

end<-model_forecast+sez+trend_vector #24 period forecasts

print(end)





