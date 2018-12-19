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
library(GGally)
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
anyNA(power)

#-------------------------------------------Forecasting monthly energy consumption--------------------------------------
#forecast global active energy, forecast global reactive energy, forecast sub-meter 3

#Prepare data
power[,1:2] <- power[ ,1:2]*(1000/60) #change un its to Wh
power <- mutate(power,Other = (Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3))
#new variable with the energy consumption that is not detected by the sub-meters

power_monthly <- power %>%
  filter (DateTime_GMT>=ymd_hms(20070101000000) & DateTime_GMT<=ymd_hms(20101031235900)) %>% #only take complete months
  group_by(month(DateTime_GMT),year(DateTime_GMT)) %>%
  summarise(active=sum(Global_active_power)/1000,reactive=sum(Global_reactive_power)/1000, #Kw can add
            sub_metering_1=sum(Sub_metering_1)/1000,sub_metering_2=sum(Sub_metering_2)/1000, #Wh is average because has time unit
            sub_metering_3=sum(Sub_metering_3)/1000, other=sum(Other)/1000) %>%
  arrange(`year(DateTime_GMT)`,`month(DateTime_GMT)`)

ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='kWh') +
  ggtitle("Global active power") +
  geom_line(aes(y=active),size=1,color="blue1")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='kWh') +
  ggtitle("Global reactive power") +
  geom_line(aes(y=reactive),size=1,color="blue1")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='kWh') +
  ggtitle("Energy consumption by different sub-meters") +
  geom_line(aes(y=sub_metering_1),size=1,color="blue1")+
  geom_line(aes(y=sub_metering_2),size=1,color="red1")+
  geom_line(aes(y=sub_metering_3),size=1,color="green1")+
  geom_line(aes(y=other),size=1,color="yellow1")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

#all together, needs legend
ggplot(power_monthly, aes(x=factor(`month(DateTime_GMT)`),group=1)) +
  labs(x='Month', y='kWh') +
  ggtitle("Energy consumption by different sub-meters") +
  geom_line(aes(y=sub_metering_1),size=1,color="blue1")+
  geom_line(aes(y=sub_metering_2),size=1,color="red1")+
  geom_line(aes(y=sub_metering_3),size=1,color="green1")+
  geom_line(aes(y=other),size=1,color="yellow1")+
  geom_line(aes(y=active),size=1,color="violet")+
  geom_line(aes(y=reactive),size=1,color="orange")+
  facet_grid(.~factor(`year(DateTime_GMT)`))+
  theme_dark()

#time series
power_monthly_TS <- ts(power_monthly[,3:8], frequency=12, start=c(2007,1))
autoplot(power_monthly_TS, facet=TRUE)
ggseasonplot(power_monthly_TS[,1],year.labels=T) #overlays year, same all years
ggseasonplot(power_monthly_TS[,1],polar=T) #round plot
#comparison (GGally library)
ggpairs(as.data.frame(power_monthly_TS)) #correlation between variables

#split training and testing
training_monthly_TS2 <- window(power_monthly_TS,start=c(2007,1),end=c(2009,10))
testing_monthly_TS2 <- window(power_monthly_TS,start=c(2009,11))

#--------------------------Forecasting global energy consumption----------------------------------------------------------
#components for global active power
active_monthly_components <- decompose(power_monthly_TS[,1]) #estimated variables: seasonal, trend and irregular 
autoplot(active_monthly_components)

#Holt-Winters 
training_active_monthly_HW <- HoltWinters(training_monthly_TS2[,1])
training_active_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.18 seasonal component based on recent observations, only seasonal component is important
plot(training_active_monthly_HW)
#forecast
testing_active_power_monthly <- testing_monthly_TS2[,1]
testing_active_monthly_HW_forecasts <- forecast(training_active_monthly_HW,h=12) #forecast testing
autoplot(testing_active_monthly_HW_forecasts)+
  autolayer(testing_active_power_monthly, series="True")
checkresiduals(testing_active_monthly_HW_forecasts)
accuracy(testing_active_monthly_HW_forecasts,testing_active_power_monthly)
 #RMESE=83.04364, MAE=73.04880, MAPE=10.62258(relative error) MASE=0.7683290
#check 80% and 95% confidence interval
testing_active_monthly_HW_forecasts$upper-testing_active_monthly_HW_forecasts$lower
#85%:265.6856 95%:406.331

#Linear regression
training_active_linear <- tslm(training_monthly_TS2[,1] ~ trend + season)
summary(training_active_linear)
#Forecast
training_linear_forecast <- forecast(training_active_linear,h=12) #forecast next year
autoplot(training_linear_forecast)+
  autolayer(testing_active_power_monthly, series="True")
checkresiduals(training_linear_forecast)
accuracy(training_linear_forecast,testing_active_power_monthly)
#RMSE=69.95567 MAE=63.75385  MAPE=8.492804 MASE=0.6705645
#check 80% and 95% confidence interval
training_linear_forecast$upper-training_linear_forecast$lower
#80%:310.5850 95%:488.1370

#ARIMA model
#differencing a Time Series, find d
training_diff <- diff(training_monthly_TS[,1], difference=1)
plot.ts(training_diff) #still some stationary, OK, d=1
#find p.
Pacf(training_diff, lag.max=20) #p=1, p=0? significance lag8?
#find q
Acf(training_diff, lag.max=20) #q=0? just few in lag8
#calculate automatically, need to be whole dataset 
auto.arima(power_monthly_TS[,1],seasonal = TRUE) #(0,0,0)(1,1,0), needs to be the whole TS
#arima model 
training_active_monthly_arima <- arima(training_monthly_TS2[,1], order=c(0,0,0), seasonal=c(1,1,0)) # fit an ARIMA(0,0,0)(1,1,0) model
#forecast testing arima
testing_active_monthly_arima <- forecast(training_active_monthly_arima,h=12) #forecast next year
autoplot(testing_active_monthly_arima)+
  autolayer(testing_active_power_monthly, series="True")
checkresiduals(testing_active_monthly_arima)
accuracy(testing_active_monthly_arima,testing_active_power_monthly)
# RMSE=81.77564 MAE=71.46872 MAPE=10.003528 MASE=0.7517097
#check 80% and 95% confidence interval
testing_active_monthly_arima$upper-testing_active_monthly_arima$lower
# 80%:233.2303 95%:356.6949

#comparison 3 methods
autoplot(power_monthly_TS[,1]) +
  autolayer(testing_active_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_active_monthly_arima, series="ARIMA", PI=FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))
#linear regression best error metrics, but ARIMA narrower confidence



#------------------------Forecasting monthly global reactive energy consumption--------------------------------------

