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
library(opera)
library(imputeTS)
library(xts)
library(forecast)

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
power <- na.locf(power)# fromLast = FALSE) 
anyNA(power)

#-------------------------------------------Forecasting monthly energy consumption--------------------------------------
#forecast global active energy, forecast global reactive energy, forecast sub-meter 3

#Prepare data
power[,1:2] <- power[ ,1:2]*(1000/60) #change units to Wh
power <- mutate(power,Other = (Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3))
#new variable with the energy consumption that is not detected by the sub-meters

power_monthly <- power %>%
  filter (DateTime_GMT>=ymd_hms(20070101000000) & DateTime_GMT<=ymd_hms(20101031235900)) %>% #only take complete months
  group_by(month(DateTime_GMT),year(DateTime_GMT)) %>%
  summarise(active=sum(Global_active_power)/1000,reactive=sum(Global_reactive_power)/1000, 
            sub_metering_1=sum(Sub_metering_1)/1000,sub_metering_2=sum(Sub_metering_2)/1000, 
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
 #RMSE=83.04364, MAE=73.04880, MAPE=10.62258(relative error) MASE=0.7683290
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
#linear regression best error metrics, but ARIMA narrower confidence, use ARIMA (0,0,0)(1,1,0)

#Forecasting with ARIMA Model
active_monthly_arima <- arima(power_monthly_TS[,1], order=c(0,0,0), seasonal=c(1,1,0)) # fit an ARIMA(0,0,0)(1,1,0) model
active_monthly_arima_forecast <- forecast(active_monthly_arima,h=12) #forecast next year
autoplot(active_monthly_arima_forecast)+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly energy consumption") +
  theme_bw()
checkresiduals(active_monthly_arima_forecast)
accuracy(active_monthly_arima_forecast)
# RMSE=78.34517 MAE=53.1985 MAPE=9.060036 MASE=0.5854239
#check 80% and 95% confidence interval
active_monthly_arima_forecast$upper-active_monthly_arima_forecast$lower
# 80%:233 95%:357

#------------------------Forecasting monthly global reactive energy consumption--------------------------------------
#components for global active power
reactive_monthly_components <- decompose(power_monthly_TS[,2]) #estimated variables: seasonal, trend and irregular 
autoplot(reactive_monthly_components)

#Holt-Winters 
training_reactive_monthly_HW <- HoltWinters(training_monthly_TS2[,2])
training_reactive_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 1 seasonal component based on recent observations,
plot(training_reactive_monthly_HW)
#forecast
testing_reactive_power_monthly <- testing_monthly_TS2[,2]
testing_reactive_monthly_HW_forecasts <- forecast(training_reactive_monthly_HW,h=12) #forecast testing
autoplot(testing_reactive_monthly_HW_forecasts)+
  autolayer(testing_reactive_power_monthly, series="True")
checkresiduals(testing_reactive_monthly_HW_forecasts)
accuracy(testing_reactive_monthly_HW_forecasts,testing_reactive_power_monthly)
#RMESE=23.20108 MAE=14.993927 MAPE=17.41535 MASE=1.0535130
#check 80% and 95% confidence interval
testing_reactive_monthly_HW_forecasts$upper-testing_reactive_monthly_HW_forecasts$lower
#85%:35.96249 95%54.99989

#Linear regression
training_reactive_linear <- tslm(training_monthly_TS2[,2] ~ trend + season)
summary(training_reactive_linear)
#Forecast
training_reactive_linear_forecast <- forecast(training_reactive_linear,h=12) #forecast next year
autoplot(training_reactive_linear_forecast)+
  autolayer(testing_reactive_power_monthly, series="True")
checkresiduals(training_reactive_linear_forecast)
accuracy(training_reactive_linear_forecast,testing_reactive_power_monthly)
#RMSE=14.74467 MAE=10.991128 MAPE=12.65401 MASE=0.7722658
#check 80% and 95% confidence interval
training_reactive_linear_forecast$upper-training_reactive_linear_forecast$lower
#80%:46.9 95%:74

#ARIMA model
#calculate automatically
auto.arima(power_monthly_TS[,2],seasonal = TRUE) #(0,0,1)(0,0,1), needs to be the whole TS
#arima model 
training_reactive_monthly_arima <- arima(training_monthly_TS2[,2], order=c(0,0,1), seasonal=c(0,0,1)) 
testing_reactive_monthly_arima <- forecast(training_reactive_monthly_arima,h=12) #forecast next year
autoplot(testing_reactive_monthly_arima)+
  autolayer(testing_reactive_power_monthly, series="True")
checkresiduals(testing_reactive_monthly_arima)
accuracy(testing_reactive_monthly_arima,testing_reactive_power_monthly)
# RMSE=15.77356 MAE=12.389232 MAPE=13.81878 MASE=0.8705002
#check 80% and 95% confidence interval
testing_reactive_monthly_arima$upper-testing_reactive_monthly_arima$lower
# 80%: 36.43344 95%:55.46

#comparison 3 methods
autoplot(power_monthly_TS[,2]) +
  autolayer(testing_reactive_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_reactive_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_reactive_monthly_arima, series="ARIMA", PI=FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))
#linear regression best error metrics, but ARIMA narrower confidence.
#linear regression real data are inside the confidence intervals, use linear regression

#combine models linear regression and ARIMA with opera package
reactive_models <- cbind(ARIMA=testing_reactive_monthly_arima$mean, LR=training_reactive_linear_forecast$mean)
MLpol0_reactive <- mixture(model = "MLpol", loss.type = "square")
weights_reactive <- predict(MLpol0_reactive, reactive_models, testing_monthly_TS2[,2], type='weights') #to see the wheight of each model
reactive_forecast <- ts(predict(MLpol0_reactive, reactive_models, testing_monthly_TS2[,2], type='response'), start=c(2009,11), freq=12)
autoplot(power_monthly_TS[,2]) +
  autolayer(testing_reactive_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_reactive_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_reactive_monthly_arima, series="ARIMA", PI=FALSE) +
  autolayer(reactive_forecast, series="mixture")+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

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

#Forecasting with ARIMA
reactive_monthly_arima <- arima(power_monthly_TS[,2], order=c(0,0,1), seasonal=c(0,0,1)) 
reactive_monthly_arima_forecast <- forecast(reactive_monthly_arima,h=12) #forecast next year
autoplot(reactive_monthly_arima_forecast)+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly reactive energy") +
  theme_bw()
accuracy(reactive_monthly_arima_forecast)
# RMSE=12.48428 MAE=9.79477 MAPE=11. MASE=0.6596075
#check 80% and 95% confidence interval
reactive_monthly_arima_forecast$upper-reactive_monthly_arima_forecast$lower
# 80%: 38 95%:59

#use ARIMA

#------------------------Forecasting monthly sub-meter3 energy consumption--------------------------------------
#components for sub-meter3
meter3_monthly_components <- decompose(power_monthly_TS[,5]) #estimated variables: seasonal, trend and irregular 
autoplot(meter3_monthly_components)

#Holt-Winters 
training_meter3_monthly_HW <- HoltWinters(training_monthly_TS2[,5])
training_meter3_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.445 seasonal component based on recent observations,
plot(training_meter3_monthly_HW)
#forecast
testing_meter3_power_monthly <- testing_monthly_TS2[,5]
testing_meter3_monthly_HW_forecasts <- forecast(training_meter3_monthly_HW,h=12) #forecast testing
autoplot(testing_meter3_monthly_HW_forecasts)+
  autolayer(testing_meter3_power_monthly, series="True")
checkresiduals(testing_meter3_monthly_HW_forecasts)
accuracy(testing_meter3_monthly_HW_forecasts,testing_meter3_power_monthly)
#RMSE=44.0321 MAE=33.93463 MAPE=12.05935 MASE=0.7012322
#check 80% and 95% confidence interval
testing_meter3_monthly_HW_forecasts$upper-testing_meter3_monthly_HW_forecasts$lower
#85%:116.1997 95%177.7121

#Linear regression
training_meter3_linear <- tslm(training_monthly_TS2[,5] ~ trend + season)
summary(training_meter3_linear)
#Forecast
training_meter3_linear_forecast <- forecast(training_meter3_linear,h=12) #forecast next year
autoplot(training_meter3_linear_forecast)+
  autolayer(testing_meter3_power_monthly, series="True")
checkresiduals(training_meter3_linear_forecast)
accuracy(training_meter3_linear_forecast,testing_meter3_power_monthly)
#RMSE=41.76438 MAE=35.05885 MAPE=12.23261 MASE=0.724463
#check 80% and 95% confidence interval
training_meter3_linear_forecast$upper-training_meter3_linear_forecast$lower
#80%:131 95%:205

#ARIMA model
#calculate automatically
auto.arima(power_monthly_TS[,5],seasonal = TRUE) #(0,0,0)(1,1,0), needs to be the whole TS
#arima model 
training_meter3_monthly_arima <- arima(training_monthly_TS2[,5], order=c(0,0,0), seasonal=c(1,1,0)) 
testing_meter3_monthly_arima <- forecast(training_meter3_monthly_arima,h=12) #forecast next year
autoplot(testing_meter3_monthly_arima)+
  autolayer(testing_meter3_power_monthly, series="True")
checkresiduals(testing_meter3_monthly_arima)
accuracy(testing_meter3_monthly_arima,testing_meter3_power_monthly)
# RMSE=55.30034 MAE=44.46019 MAPE=13.24964 MASE=0.9187345
#check 80% and 95% confidence interval
testing_meter3_monthly_arima$upper-testing_meter3_monthly_arima$lower
# 80%: 137 95%:209

#comparison 3 methods
autoplot(power_monthly_TS[,5]) +
  autolayer(testing_meter3_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_meter3_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_meter3_monthly_arima, series="ARIMA", PI=FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))
#Holt-Winters better metrics and narrower confidence interval

#Forecasting with Holt-Winters
meter3_monthly_HW <- HoltWinters(power_monthly_TS[,5])
meter3_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.54 seasonal component based on recent observations, changed a little from the training
plot(meter3_monthly_HW)
meter3_monthly_HW_forecasts <- forecast(meter3_monthly_HW,h=12) #forecast testing
autoplot(meter3_monthly_HW_forecasts)+
  geom_line(size=1,color="red")+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly energy consumption in boiler+AC") +
  theme_bw()
checkresiduals(testing_meter3_monthly_HW_forecasts)
accuracy(meter3_monthly_HW_forecasts)
#RMSE=44.11075 MAE=32.57227 MAPE=15.84121 MASE=0.6933442
#check 80% and 95% confidence interval
meter3_monthly_HW_forecasts$upper-meter3_monthly_HW_forecasts$lower
#85%:114.546 95%:175.183

#------------------------Forecasting monthly sub-meter1 energy consumption--------------------------------------
#components for sub-meter1
meter1_monthly_components <- decompose(power_monthly_TS[,3]) #estimated variables: seasonal, trend and irregular 
autoplot(meter1_monthly_components)

#Holt-Winters 
training_meter1_monthly_HW <- HoltWinters(training_monthly_TS2[,3])
training_meter1_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0 seasonal component based on past observations,
plot(training_meter1_monthly_HW)
#forecast
testing_meter1_power_monthly <- testing_monthly_TS2[,3]
testing_meter1_monthly_HW_forecasts <- forecast(training_meter1_monthly_HW,h=12) #forecast testing
autoplot(testing_meter1_monthly_HW_forecasts)+
  autolayer(testing_meter1_power_monthly, series="True")
checkresiduals(testing_meter1_monthly_HW_forecasts)
accuracy(testing_meter1_monthly_HW_forecasts,testing_meter1_power_monthly)
#RMSE=12.26473 MAE=10.097473 MAPE=31.65231 MASE=0.8160294
#check 80% and 95% confidence interval
testing_meter1_monthly_HW_forecasts$upper-testing_meter1_monthly_HW_forecasts$lower
#85%:36 95%:56

#Linear regression
training_meter1_linear <- tslm(training_monthly_TS2[,3] ~ trend + season)
summary(training_meter1_linear)
#Forecast
training_meter1_linear_forecast <- forecast(training_meter1_linear,h=12) #forecast next year
autoplot(training_meter1_linear_forecast)+
  autolayer(testing_meter1_power_monthly, series="True")
checkresiduals(training_meter1_linear_forecast)
accuracy(training_meter1_linear_forecast,testing_meter1_power_monthly)
#RMSE=10.988005 MAE=9.358829 MAPE=27.74643 MASE=0.7563357
#check 80% and 95% confidence interval
training_meter1_linear_forecast$upper-training_meter1_linear_forecast$lower
#80%:41 95%:65

#ARIMA model
#calculate automatically
auto.arima(power_monthly_TS[,3],seasonal = TRUE) #(0,0,0)(1,0,0), needs to be the whole TS
#arima model 
training_meter1_monthly_arima <- arima(training_monthly_TS2[,3], order=c(0,0,0), seasonal=c(1,0,0)) 
testing_meter1_monthly_arima <- forecast(training_meter1_monthly_arima,h=12) #forecast next year
autoplot(testing_meter1_monthly_arima)+
  autolayer(testing_meter1_power_monthly, series="True")
checkresiduals(testing_meter1_monthly_arima)
accuracy(testing_meter1_monthly_arima,testing_meter1_power_monthly)
# RMSE=15.62575 MAE=12.44513 MAPE=48.71549 MASE=1.0057554
#check 80% and 95% confidence interval
testing_meter1_monthly_arima$upper-testing_meter1_monthly_arima$lower
# 80%: 35 95%:53

#comparison 3 methods
autoplot(power_monthly_TS[,3]) +
  autolayer(testing_meter1_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_meter1_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_meter1_monthly_arima, series="ARIMA", PI=FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

#combine models with opera package
meter1_models <- cbind(HW=testing_meter1_monthly_HW_forecasts$mean, ARIMA=testing_meter1_monthly_arima$mean, LR=training_meter1_linear_forecast$mean)
MLpol0_meter1 <- mixture(model = "MLpol", loss.type = "square")
weights_meter1 <- predict(MLpol0_meter1, meter1_models, testing_monthly_TS2[,3], type='weights') #to see the wheight of each model
meter1_forecast <- ts(predict(MLpol0_meter1, meter1_models, testing_monthly_TS2[,3], type='response'), start=c(2009,11), freq=12)
autoplot(power_monthly_TS[,3]) +
  autolayer(meter1_forecast, series="mixture")+
  autolayer(testing_meter1_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_meter1_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_meter1_monthly_arima, series="ARIMA", PI=FALSE) 
#no much improve, use HW

meter1_monthly_HW <- HoltWinters(power_monthly_TS[,3])
meter1_monthly_HW
#alpha: 0 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.20 seasonal component based on past and recent observations,
plot(meter1_monthly_HW)
meter1_monthly_HW_forecasts <- forecast(meter1_monthly_HW,h=12) #forecast testing
autoplot(meter1_monthly_HW_forecasts)+
  geom_line(size=1,color="red")+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly energy consumption in the Kitchen") +
  theme_bw()
checkresiduals(meter1_monthly_HW_forecasts)
accuracy(meter1_monthly_HW_forecasts)
#RMSE=13.1304 MAE=10.04051 MAPE=47.1713 MASE=0.778721
#check 80% and 95% confidence interval
meter1_monthly_HW_forecasts$upper-meter1_monthly_HW_forecasts$lower
#85%:34 95%:52

#------------------------Forecasting monthly sub-meter2 energy consumption--------------------------------------
#components for sub-meter2
meter2_monthly_components <- decompose(power_monthly_TS[,4]) #estimated variables: seasonal, trend and irregular 
autoplot(meter2_monthly_components)

#Holt-Winters 
training_meter2_monthly_HW <- HoltWinters(training_monthly_TS2[,4])
training_meter2_monthly_HW
#alpha: 0.1 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0 seasonal component based on past observations,
plot(training_meter2_monthly_HW)
#forecast
testing_meter2_power_monthly <- testing_monthly_TS2[,4]
testing_meter2_monthly_HW_forecasts <- forecast(training_meter2_monthly_HW,h=12) #forecast testing
autoplot(testing_meter2_monthly_HW_forecasts)+
  autolayer(testing_meter2_power_monthly, series="True")
checkresiduals(testing_meter2_monthly_HW_forecasts)
accuracy(testing_meter2_monthly_HW_forecasts,testing_meter2_power_monthly)
#RMSE=16.06957 MAE=13.86413 MAPE=30.63604 MASE=0.7745400
#check 80% and 95% confidence interval
testing_meter2_monthly_HW_forecasts$upper-testing_meter2_monthly_HW_forecasts$lower
#85%:36 95%:55

#Linear regression
training_meter2_linear <- tslm(training_monthly_TS2[,4] ~ trend + season)
summary(training_meter2_linear)
#Forecast
training_meter2_linear_forecast <- forecast(training_meter2_linear,h=12) #forecast next year
autoplot(training_meter2_linear_forecast)+
  autolayer(testing_meter2_power_monthly, series="True")
checkresiduals(training_meter2_linear_forecast)
accuracy(training_meter2_linear_forecast,testing_meter2_power_monthly)
#RMSE=11.74129 MAE=10.481984 MAPE=22.85991 MASE=0.5855917
#check 80% and 95% confidence interval
training_meter2_linear_forecast$upper-training_meter2_linear_forecast$lower
#80%:43 95%:68

#ARIMA model
#calculate automatically
auto.arima(power_monthly_TS[,4],seasonal = TRUE) #(2,1,0)(1,0,0), needs to be the whole TS
#arima model 
training_meter2_monthly_arima <- arima(training_monthly_TS2[,4], order=c(2,1,0), seasonal=c(1,0,0)) 
testing_meter2_monthly_arima <- forecast(training_meter2_monthly_arima,h=12) #forecast next year
autoplot(testing_meter2_monthly_arima)+
  autolayer(testing_meter2_power_monthly, series="True")
checkresiduals(testing_meter2_monthly_arima)
accuracy(testing_meter2_monthly_arima,testing_meter2_power_monthly)
# RMSE=10.26684  MAE=8.940694 MAPE=22.52793 MASE=0.4994852
#check 80% and 95% confidence interval
testing_meter2_monthly_arima$upper-testing_meter2_monthly_arima$lower
# 80%: 72 95%:111

#comparison 3 methods
autoplot(power_monthly_TS[,4]) +
  autolayer(testing_meter2_monthly_HW_forecasts, series="HoltWinters", PI=FALSE) +
  autolayer(training_meter2_linear_forecast, series="Linear Regression", PI=FALSE) +
  autolayer(testing_meter2_monthly_arima, series="ARIMA", PI=FALSE) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecasts for monthly energy consumption") +
  guides(colour=guide_legend(title="Forecast"))

#Choose linear regression
meter2_linear <- tslm(power_monthly_TS[,4] ~ trend + season)
summary(meter2_linear)
#Forecast
meter2_linear_forecast <- forecast(meter2_linear,h=12) #forecast next year
autoplot(meter2_linear_forecast)+
  xlab("Year") + ylab("kWh") +
  ggtitle("Forecast for monthly energy consumption in the laundry room") +
  theme_bw()
checkresiduals(meter2_linear_forecast)
accuracy(meter2_linear_forecast)
#RMSE=9.488194 MAE=7.224636 MAPE=14.93769 MASE=0.4743367
#check 80% and 95% confidence interval
meter2_linear_forecast$upper-meter2_linear_forecast$lower
#80%:34 95%:53

#Holt Winters
meter2_monthly_HW <- HoltWinters(power_monthly_TS[,4])
meter2_monthly_HW
#alpha: 0.18 level based on both recent and past observations
#beta: 0 slope of the trend based on past and recent observations
#gamma: 0.45 seasonal component based on past observations,
plot(meter2_monthly_HW)
#forecast
meter2_monthly_HW_forecasts <- forecast(meter2_monthly_HW,h=12)
autoplot(meter2_monthly_HW_forecasts)

checkresiduals(meter2_monthly_HW_forecasts)
accuracy(meter2_monthly_HW_forecasts)
#RMSE=12.48525 MAE=9.676073 MAPE=21.84323 MASE=0.6352869
#check 80% and 95% confidence interval
meter2_monthly_HW_forecasts$upper-meter2_monthly_HW_forecasts$lower
#85%:37 95%:58

#Use Linear regression

#-----------------------------------Descriptive for the presentation------------------------------
#different sub-meters during 2 days by hour
labels <- c(Other = "Other", Sub_metering_1 = "Kitchen", Sub_metering_2="Laundry room", Sub_metering_3="Boiler+AC")
power %>%
  filter (DateTime_TC >= ymd_hms(20100322000000) & DateTime_TC <= ymd_hms(20100323235900)) %>%
  gather (Meter, Wh, "Sub_metering_1", "Sub_metering_2","Sub_metering_3","Other") %>%
  group_by(hour(DateTime_TC),weekdays(DateTime_TC), Meter) %>%
  summarise(kWh=sum(Wh)/1000) %>% 
  ggplot( aes(x=factor(`hour(DateTime_TC)`),kWh,group=Meter,color=Meter)) +
  labs(x='Hour of the day', y='kWh') +
  ggtitle("Energy usage by each sub-meter") +
  geom_line(aes(color=Meter),size=1)+
  facet_grid(factor(`weekdays(DateTime_TC)`)~Meter,labeller=labeller(Meter = labels))+
  theme_dark()+
  theme(legend.position = "none")

#proportion of each submeter-yearly
power %>%
  filter(year(DateTime_TC) != 2006) %>%
  gather (Meter, Wh, "Sub_metering_1", "Sub_metering_2","Sub_metering_3","Other") %>%
  group_by(year(DateTime_TC), Meter) %>% 
  summarise(kWh=sum(Wh)/1000) %>%
  ggplot(aes(x=factor(`year(DateTime_TC)`), kWh, group=Meter,fill=Meter)) +
  labs(x='Year', y='Proportion of Energy Usage') +
  ggtitle('Yearly Energy Consumption') +
  geom_bar(position='fill',stat="identity") +#stat=identity to plot sum, position=fill to have proportions
  theme_dark()

#----------isolate refrigerator---------------------------------------------------
power %>%
  filter (DateTime_TC >= ymd_hms(20100322000000) & DateTime_TC <= ymd_hms(20100322235900)) %>%
  group_by(hour(DateTime_TC),weekdays(DateTime_TC)) %>%
  summarise(sum=sum(Sub_metering_2)) %>%
  ggplot( aes(x=factor(`hour(DateTime_TC)`),sum,group=1)) +
  labs(x='Hour of the day', y='Wh') +
  ggtitle("Energy usage in the laundry room") +
  geom_line(color="cyan1",size=1)+
  facet_grid(.~factor(`weekdays(DateTime_TC)`))+
  theme_dark()
#no grouping
power %>%
  filter (DateTime_TC >= ymd_hms(20100323000000) & DateTime_TC <= ymd_hms(20100323235900)) %>%
  ggplot( aes(x=DateTime_TC,Sub_metering_2,group=1)) +
  labs(x='Hour of the day', y='Wh') +
  ggtitle("Energy usage in the laundry room") +
  geom_line(color="cyan1")+
  theme_dark()

#remove all values >2 then group by hour
power_fridge <- power[power$Sub_metering_2<=2, c(6,8)]
power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20100323000000) & DateTime_GMT <= ymd_hms(20100323235900)) %>%
  ggplot( aes(x=DateTime_GMT,Sub_metering_2,group=1)) +
  labs(x='Hour of the day', y='Wh') +
  ggtitle("Energy usage in the laundry room") +
  geom_line(color="cyan1")+
  theme_dark()
#include missing observations / removed with NAs
allDates <- seq(min(power_fridge$DateTime_GMT), max(power_fridge$DateTime_GMT),by="mins")
power_fridge <- merge(
  x=data.frame(DateTime_GMT=allDates),
  y=power_fridge,
  all.x=TRUE)
power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20101001000000)) %>% # & DateTime_GMT <= ymd_hms(20101023235900)) %>%
  ggplot( aes(x=DateTime_GMT,Sub_metering_2,group=1)) +
  labs(x='Hour of the day', y='Wh') +
  ggtitle("Energy usage in the laundry room") +
  geom_line(color="cyan1")+
  theme_dark()
#group by 30min
power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20100323000000) & DateTime_GMT <= ymd_hms(20100325235900)) %>%
  group_by(by30=cut(DateTime_GMT, "30 min")) %>%
  summarise(sum=sum(Sub_metering_2)) %>%
  ggplot( aes(x=by30,sum,group=1)) +
  labs(x='Time', y='Wh') +
  ggtitle("Energy usage for the refrigerator") +
  geom_line(color="cyan1",size=1)+
  theme_dark()
#impute missing values, use data from 1 oct2010 (28sept 2010 last day with missing values)
#each cycle is 5.5h

fridge_ts <- ts(power_fridge[,2],frequency=525600)
plot(fridge_ts)
fridge_ts <- na.seadec(fridge_ts,algorithm="interpolation")
plot(fridge_ts)

#return to data frame for descriptive
pred_fridge <- as.matrix(fridge_ts)
power_fridge <- cbind(power_fridge, pred_fridge)

power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20101123000000) & DateTime_GMT <= ymd_hms(20101125235900)) %>%
  group_by(by30=cut(DateTime_GMT, "30 min")) %>%
  summarise(sub2=sum(Sub_metering_2),pred=sum(pred_fridge)) %>%
  ggplot( aes(x=by30,sub2,group=1)) +
  labs(x='Time', y='Wh') +
  ggtitle("Energy usage for the refrigerator") +
  geom_line(aes(y=pred),color="red",size=1)+
  geom_line(color="cyan1",size=1)+
  theme_dark()
power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20101101000000) & DateTime_GMT <= ymd_hms(20101104235900)) %>%
  group_by(by1h=cut(DateTime_GMT, "1 hour")) %>%
  summarise(sub2=sum(Sub_metering_2),pred=sum(pred_fridge)) %>%
  ggplot( aes(x=by1h,sub2,group=1)) +
  labs(x='Time', y='Wh') +
  ggtitle("Energy usage for the refrigerator") +
  geom_line(aes(y=pred),color="red",size=1)+
  geom_line(color="cyan1",size=1)+
  theme_dark()
#grouped by 1h shows acceptable  picks


fridge_30min <- power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20101101000000)) %>%
  group_by(by30=cut(DateTime_GMT, "30 min")) %>%
  summarise(wh=sum(Sub_metering_2))

fridge_30min_all <- power_fridge %>%
  filter(DateTime_GMT != 2006) %>%
  group_by(by30=cut(DateTime_GMT, "30 min")) %>%
  summarise(wh=sum(Sub_metering_2))

fridge_h <- power_fridge %>%
  filter (DateTime_GMT >= ymd_hms(20101001000000)) %>%
  group_by(by1h=cut(DateTime_GMT, "1 hour")) %>%
  summarise(wh=sum(Sub_metering_2))
 
#ask with lubridate how many hours to calculate how many 30min periods
#do time series
#na.desc
#decompose
#forecastings

fridge_ts <- ts(fridge_30min_all[,2],frequency=17520, start=2007)
fridge_ts <- na.seadec(fridge_ts,algorithm="interpolation")
fridge_ts_decomp <- decompose(fridge_ts)
autoplot(fridge_ts_decomp)
autoplot(auto.arima(fridge_ts))


fridge_ts <- zoo(x=fridge_30min$wh, order.by = fridge_30min$by30, frequency = 11) #each pick is every 11 blocks of 30min
fridge_ts <- as.zoo(fridge_ts)
plot(fridge_ts)
#impute the missing values with the imputeTS package
fridge_ts <- na.seadec(fridge_ts,algorithm="interpolation") #error missing values????
fridge_ts <- ts(coredata(fridge_ts),frequency = 11)
fridge_ts <- na.seadec(fridge_ts,algorithm = "interpolation")
plot(fridge_ts)
#training and testing
fridge_testing <-window(fridge_ts, from=)  #30%
fridge_training <-window(fridge_ts, index = index(fridge_ts)[1:1910])    #70%
#ARIMA model works with zoo objects
auto.arima(fridge_ts,seasonal = TRUE) #(5,1,4)
#arima model 
training_fridge_arima <- arima(fridge_training, order=c(5,1,4)) 
training_fridge_arima_forecast <- forecast(training_fridge_arima,h=820) #forecast next year
autoplot(training_fridge_arima_forecast)
checkresiduals(training_fridge_arima_forecast)


