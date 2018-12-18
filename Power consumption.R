#Power consumption
#Lara Cobler Moncunill
#December 13th, 2018

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(zoo)
library(plotly)
library(VIM)
library(httr)
cat(content(GET("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R"), "text"), file="calendarHeat.R")
source("calendarHeat.R")


#Download dataset
power <- read_delim('household_power_consumption.txt', col_names = TRUE,delim=';',na='?') 

#Date and time
power <- power %>% unite("DateTime", Date, Time, sep = " ")
power$DateTime_GMT <- as.POSIXct(power$DateTime,format="%d/%m/%Y %T",tz="GMT")
power$DateTime <- NULL
power$DateTime_TC <- power$DateTime_GMT
indexTC <- (which((power$DateTime_TC>=ymd_hms(20070325020000) & power$DateTime_TC<=ymd_hms(20071028020000)) |
                    (power$DateTime_TC>=ymd_hms(20080330020000) & power$DateTime_TC<=ymd_hms(20081006020000)) |
                    (power$DateTime_TC>=ymd_hms(20090329020000) & power$DateTime_TC<=ymd_hms(20091025020000)) |
                    (power$DateTime_TC>=ymd_hms(20100328020000) & power$DateTime_TC<=ymd_hms(20101031020000)))) 
power$DateTime_TC[indexTC] <- power$DateTime_TC[indexTC]+hours(1)
#DateTime_TC is for the descriptive
#for forecasting is right to use GMT because we will goup at least by day, does not affect +/- 1h

#-----------------------------------Deal with NAs----------------------------------------------------------------------
aggr(power) #package VIM, distribution of NAs 
calendarHeat(dates=power$DateTime_GMT,values=power$Global_active_power)
#Days with missing values: 
  #28 and 29 April 2007 
  #13 and 14 June 2009
  #12 and 13 January 2010
  #20 and 21 march 2010
  #17, 18, 19, 20 and 21 August 2010
  #25, 26 and 27 September 2010
#most look like vacation days and weekends out. I assume that this long periods are 0
indexV <- (which((power$DateTime_GMT>=ymd_hms(20070428000100) & power$DateTime_GMT<=ymd_hms(20070430235900)) |
                   (power$DateTime_GMT>=ymd_hms(20090613000100) & power$DateTime_GMT<=ymd_hms(20090615235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100112000100) & power$DateTime_GMT<=ymd_hms(20100114235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100320000100) & power$DateTime_GMT<=ymd_hms(20100321235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100817000100) & power$DateTime_GMT<=ymd_hms(20100822235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100925000100) & power$DateTime_GMT<=ymd_hms(20100928235900)))) 
power[indexV,] <- power[indexV,] %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
calendarHeat(dates=power$DateTime_GMT,values=power$Global_active_power)            
#other missing values
aggr(power) 
#plot missing values
power_NA <- power[is.na(power$Global_active_power),]
summary(power_NA)
plot_na <- power_NA %>%
  plot_ly(
    x = ~DateTime_GMT,
    autobinx = FALSE, 
    autobiny = TRUE, 
    marker = list(color = "rgb(68, 68, 68)"), 
    name = "date", 
    type = "histogram", 
    xbins = list(
      end = "2010-10-24 15:35", 
      size = "W1", 
      start = "2006-12-21 11:23"
    )
  )
plot_na
#13 august 2009? 13h?
power %>%
  filter (DateTime_GMT>=ymd_hms(20090811000000) & DateTime_GMT<=ymd_hms(20090814235900)) %>%
  ggplot( aes(x=DateTime_GMT),Global_active_power,group=1) +
  labs(x='Month', y='kW') +
  ggtitle("Global active power") +
  geom_line(aes(y=Global_active_power),size=1,color="green1")
#13h with no power then, the usage id high, recover what they haven't used. Assign 0 also
indexV2 <- (which((power$DateTime_GMT>=ymd_hms(20090813000100) & power$DateTime_GMT<=ymd_hms(20090813235900)))) 
power[indexV2,] <- power[indexV2,] %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
#check 9june - 31jul 2007
power %>%
  filter (DateTime_GMT>=ymd_hms(20070608000000) & DateTime_GMT<=ymd_hms(20070610235900)) %>%
  ggplot( aes(x=DateTime_GMT),Global_active_power,group=1) +
  labs(x='Month', y='kW') +
  ggtitle("Global active power") +
  geom_line(aes(y=Global_active_power),size=1,color="green1")
#for forecasting, less 1h is not very important, can take the value just before 
power %>%
  filter (DateTime_GMT>=ymd_hms(20070714000000) & DateTime_GMT<=ymd_hms(20070716235900)) %>%
  ggplot( aes(x=DateTime_GMT),Global_active_power,group=1) +
  labs(x='Month', y='kW') +
  ggtitle("Global active power") +
  geom_line(aes(y=Global_active_power),size=1,color="green1")
#take value just before (using zoo library)
power <- na.locf(power, fromLast = FALSE) 

#------------------------Forecasting monthly global energy consumption--------------------------------------
#forecast total, reactive, sub-meter3 and rest?

#Prepare data
power <- mutate(power,Other = ((Global_active_power*(1000/60)) - Sub_metering_1 - Sub_metering_2 - Sub_metering_3))
 #new variable with the energy consumption that is not detected by the sub-meters
power_monthly <- power %>%
  filter (DateTime_GMT>=ymd_hms(20070101000000) & DateTime_GMT<=ymd_hms(20101031235900)) %>% #only take complete months
  group_by(month(DateTime_GMT),year(DateTime_GMT)) %>%
  summarise(active=sum(Global_active_power),reactive=sum(Global_reactive_power), #Kw can add
            sub_metering_1=mean(Sub_metering_1),sub_metering_2=mean(Sub_metering_2), #Wh is average because has time unit
            sub_metering_3=mean(Sub_metering_3), other=mean(Other)) %>%
  arrange(`year(DateTime_GMT)`,`month(DateTime_GMT)`)

ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='kW') +
  ggtitle("Global active power") +
  geom_line(aes(y=reactive),size=1,color="blue1")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='kW') +
  ggtitle("Global reactive power") +
  geom_line(aes(y=reactive),size=1,color="blue1")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='Wh') +
  ggtitle("Energy consumption by different sub-meters") +
  geom_line(aes(y=sub_metering_1),size=1,color="blue1")+
  geom_line(aes(y=sub_metering_2),size=1,color="red1")+
  geom_line(aes(y=sub_metering_3),size=1,color="green1")+
  geom_line(aes(y=other),size=1,color="yellow1")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

#split training and testing
training_power_monthly <- power_monthly[1:34,]
testing_power_monthly <- power_monthly[35:46,] #12 observations, to have one full cycle

#time series
power_monthly_TS <- ts(power_monthly[,3:8], frequency=12, start=c(2007,1))
plot.ts(power_monthly_TS)
training_monthly_TS <- ts(training_power_monthly[,3:8], frequency=12, start=c(2007,1))
testing_monthly_TS <- ts(testing_power_monthly[,3:8], frequency=12, start=c(2009,11))

#components for global active power
training_active_monthly_components <- decompose(training_monthly_TS[,1]) #estimated variables: seasonal, trend and irregular 
training_active_monthly_components$seasonal
plot(training_active_monthly_components)

#Holt-Winters Exponential Smoothing total active power
#described by additive model, with increasing or decreasing trend and seasonality
#short-term forecasts
training_active_monthly_forecasts <- HoltWinters(training_monthly_TS[,1])
training_active_monthly_forecasts
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.18 seasonal component based on recent observations, only seasonal component is important
plot(training_active_monthly_forecasts)
#forecast
testing_active_power_monthly <- testing_monthly_TS[,1]
training_active_monthly_forecasts2 <- forecast(training_active_monthly_forecasts,h=12) #forecast next year
plot(training_active_monthly_forecasts2) # 80% and 95% prediction interval
lines(testing_active_power_monthly,col="red")
accuracy(training_active_monthly_forecasts2,testing_active_power_monthly)
Acf(training_active_monthly_forecasts2$residuals, lag.max=20)  #autocorrelation
Pacf(training_active_monthly_forecasts2$residuals, lag.max=20) #partial autocorrelation
plot.ts(activepower_monthly_forecasts2$residuals) # make a time plot, good???
hist(activepower_monthly_forecasts2$residuals) # make a histogram, not exactly centered??
checkresiduals(activepower_monthly_forecasts2)

#Linear regression
training_linear <- tslm(training_monthly_TS[,1] ~ trend + season)
summary(training_linear)
#Plot fitted vs actual
training_linear_forecast <- forecast(training_linear,h=12) #forecast next year
plot(training_linear_forecast) # 80% and 95% prediction interval
lines(testing_active_power_monthly,col="red")
accuracy(training_linear_forecast,testing_active_power_monthly)
Acf(training_linear_forecast$residuals, lag.max=20)  #autocorrelation
Pacf(training_linear_forecast$residuals, lag.max=20) #partial autocorrelation
plot.ts(training_linear_forecast$residuals) # make a time plot, good???
hist(training_linear_forecast$residuals) # make a histogram, not exactly centered??
checkresiduals(training_linear_forecast)
#better the linear than the HoltWinters

#ARIMA model
#differencing a Time Series, find d
training_diff <- diff(training_monthly_TS[,1], difference=1)
plot.ts(training_diff) #still some stationary, OK, d=1
#find p.
Pacf(training_diff, lag.max=20) #p=1, p=0? significance lag8?
#find q
Acf(training_diff, lag.max=20) #q=0? just few in lag8
#calculate automatically
auto.arima(training_monthly_TS[,1],seasonal = TRUE) #(0,0,0)(0,1,0)
#arima model 
training_active_monthly_arima <- arima(training_monthly_TS[,1], order=c(0,0,0), seasonal=c(0,1,0)) # fit an ARIMA(0,0,0)(0,1,0) model
#forecast testing arima
testing_active_monthly_arima <- forecast(training_active_monthly_arima,h=12) #forecast next year
plot(testing_active_monthly_arima) # 80% and 95% prediction interval
lines(testing_active_power_monthly,col="red")
accuracy(testing_active_monthly_arima,testing_active_power_monthly)
#Acf(testing_active_monthly_arima1$residuals, lag.max=20)  #autocorrelation
#Pacf(testing_active_monthly_arima1$residuals, lag.max=20) #partial autocorrelation
#plot.ts(testing_active_monthly_arima1$residuals) # make a time plot,no all seasonality
#hist(testing_active_monthly_arima1$residuals) # make a histogram, 
checkresiduals(testing_active_monthly_arima)


##best model is the linear regression

