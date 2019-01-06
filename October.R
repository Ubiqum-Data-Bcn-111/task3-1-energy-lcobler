#Power consumption, forcasting for october 2010
#code from power consumption.R adapted to forcast october 2010
#Lara Cobler Moncunill
#January 3rd, 2019

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(zoo)
library(plotly)
library(GGally)
library(VIM)
library(httr)
cat(content(GET("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R"), "text"), file="calendarHeat.R")
source("calendarHeat.R")
library(opera)

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
anyNA(power)

#-------------------------------------------Forecasting monthly energy consumption--------------------------------------
#forecast global active energy, forecast global reactive energy, forecast sub-meter 3

#Prepare data
power[,1:2] <- power[ ,1:2]*(1000/60) #change units to Wh
power <- mutate(power,Other = (Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3))
#new variable with the energy consumption that is not detected by the sub-meters

#monthly grouping until september to forecast october
power_monthly <- power %>%
  filter (DateTime_GMT>=ymd_hms(20070101000000) & DateTime_GMT<=ymd_hms(20100930235900)) %>% #only take complete months
  group_by(month(DateTime_GMT),year(DateTime_GMT)) %>%
  summarise(active=sum(Global_active_power)/1000,reactive=sum(Global_reactive_power)/1000, 
            sub_metering_1=sum(Sub_metering_1)/1000,sub_metering_2=sum(Sub_metering_2)/1000, 
            sub_metering_3=sum(Sub_metering_3)/1000, other=sum(Other)/1000) %>%
  arrange(`year(DateTime_GMT)`,`month(DateTime_GMT)`)

#time series
power_monthly_TS <- ts(power_monthly[,3:8], frequency=12, start=c(2007,1))
autoplot(power_monthly_TS, facet=TRUE)

#comparison (GGally library)
ggpairs(as.data.frame(power_monthly_TS)) #correlation between variables


#--------------------------Forecasting global energy consumption----------------------------------------------------------
#Forecasting with ARIMA Model (see power consumption.R)
active_monthly_arima <- arima(power_monthly_TS[,1], order=c(0,0,0), seasonal=c(1,1,0)) # fit an ARIMA(0,0,0)(1,1,0) model
active_monthly_arima_forecast <- forecast(active_monthly_arima,h=12) #forecast next year
autoplot(active_monthly_arima_forecast)+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly energy consumption") +
  theme_bw()+
  theme(legend.position = "none")
checkresiduals(active_monthly_arima_forecast)
accuracy(active_monthly_arima_forecast)
# RMSE=78.34517 MAE=53.1985 MAPE=9.060036 MASE=0.5854239
#check 80% and 95% confidence interval
active_monthly_arima_forecast$upper-active_monthly_arima_forecast$lower
# 80%:233 95%:357

#calculate cost of november
active_monthly_arima_forecast$mean[2] * 0.0135

#------------------------Forecasting monthly global reactive energy consumption--------------------------------------

#Forecasting with linear regression
reactive_linear <- tslm(power_monthly_TS[,2] ~ trend + season)
summary(reactive_linear)
#Forecast
reactive_linear_forecast <- forecast(reactive_linear,h=12) #forecast next year
autoplot(reactive_linear_forecast)+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly reactive energy") +
  theme_bw()+
  theme(legend.position = "none")
checkresiduals(reactive_linear_forecast)
accuracy(reactive_linear_forecast)
#RMSE=11.3821 MAE=8.893866 MAPE=9.967836 MASE=0.5989381
#check 80% and 95% confidence interval
reactive_linear_forecast$upper-reactive_linear_forecast$lower
#80%:42 95%:65

#------------------------Forecasting monthly sub-meter3 energy consumption--------------------------------------
#Forecasting with Holt-Winters
meter3_monthly_HW <- HoltWinters(power_monthly_TS[,5])
meter3_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.55 seasonal component based on recent observations, changed a little from the training
plot(meter3_monthly_HW)
meter3_monthly_HW_forecasts <- forecast(meter3_monthly_HW,h=12) #forecast testing
autoplot(meter3_monthly_HW_forecasts)+
  geom_line(size=1,color="red")+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly energy consumption in boiler+AC") +
  theme_bw()+
  theme(legend.position = "none")
checkresiduals(testing_meter3_monthly_HW_forecasts)
accuracy(meter3_monthly_HW_forecasts)
#RMSE=44.11075 MAE=32.57227 MAPE=15.84121 MASE=0.6933442
#check 80% and 95% confidence interval
meter3_monthly_HW_forecasts$upper-meter3_monthly_HW_forecasts$lower
#85%:114.546 95%:175.183

#------------------------Forecasting monthly sub-meter1 energy consumption--------------------------------------
meter1_monthly_HW <- HoltWinters(power_monthly_TS[,3])
meter1_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.13 seasonal component based on past and recent observations,
plot(meter1_monthly_HW)
meter1_monthly_HW_forecasts <- forecast(meter1_monthly_HW,h=12) #forecast testing
autoplot(meter1_monthly_HW_forecasts)
checkresiduals(meter1_monthly_HW_forecasts)
accuracy(meter1_monthly_HW_forecasts)
#RMSE=13.1304 MAE=10.04051 MAPE=47.1713 MASE=0.778721
#check 80% and 95% confidence interval
meter1_monthly_HW_forecasts$upper-meter1_monthly_HW_forecasts$lower
#85%:34 95%:52

#------------------------Forecasting monthly sub-meter2 energy consumption--------------------------------------
#Choose linear regression
meter2_linear <- tslm(power_monthly_TS[,4] ~ trend + season)
summary(meter2_linear)
#Forecast
meter2_linear_forecast <- forecast(meter2_linear,h=12) #forecast next year
autoplot(meter2_linear_forecast)
checkresiduals(meter2_linear_forecast)
accuracy(meter2_linear_forecast)
#RMSE=9.488194 MAE=7.224636 MAPE=14.93769 MASE=0.4743367
#check 80% and 95% confidence interval
meter2_linear_forecast$upper-meter2_linear_forecast$lower
#80%:34 95%:53

#-----------------------------------Descriptive for the presentation------------------------------
power %>%
  filter (DateTime_TC >= ymd_hms(20101001000000) & DateTime_TC <= ymd_hms(20101031235900)) %>%
  group_by(hour(DateTime_TC)) %>%
  summarise(Wh=mean(Global_active_power)) %>%
  ggplot(aes(x=factor(`hour(DateTime_TC)`), Wh, group=1)) +
  labs(x='Hour of the day', y='Wh') +
  ggtitle("Energy usage hour of the day") +
  geom_bar(position='stack',stat="identity")
#top3: 20,21 and 22 within peak demand

#monthly averages
power_monthly %>%
  group_by(factor(`month(DateTime_GMT)`)) %>% 
  summarise(total=mean(active), reactive=mean(reactive), kitchen=mean(sub_metering_1), laundry=mean(sub_metering_2),
            boiler=mean(sub_metering_3))
