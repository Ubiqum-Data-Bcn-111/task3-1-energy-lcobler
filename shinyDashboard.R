#Shiny dashboard for energy consumption
#Lara Cobler Moncunill
#21 December 2018

library(flexdashboard)
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(tidyr)
library(forecast)


#Load Data
power <- read_delim('household_power_consumption.txt', col_names = TRUE,delim=';',na='?') 

#date and time
power <- power %>% unite("DateTime", Date, Time, sep = " ")
power$DateTime_GMT <- as.POSIXct(power$DateTime,format="%d/%m/%Y %T",tz="GMT")
power$DateTime <- NULL
power$DateTime_TC <- power$DateTime_GMT
indexTC <- (which((power$DateTime_TC>=ymd_hms(20070325020000) & power$DateTime_TC<=ymd_hms(20071028020000)) |
                    (power$DateTime_TC>=ymd_hms(20080330020000) & power$DateTime_TC<=ymd_hms(20081006020000)) |
                    (power$DateTime_TC>=ymd_hms(20090329020000) & power$DateTime_TC<=ymd_hms(20091025020000)) |
                    (power$DateTime_TC>=ymd_hms(20100328020000) & power$DateTime_TC<=ymd_hms(20101031020000)))) 
power$DateTime_TC[indexTC] <- power$DateTime_TC[indexTC]+hours(1)

#missing values
indexV <- (which((power$DateTime_GMT>=ymd_hms(20070428000100) & power$DateTime_GMT<=ymd_hms(20070430235900)) |
                   (power$DateTime_GMT>=ymd_hms(20090613000100) & power$DateTime_GMT<=ymd_hms(20090615235900)) |
                   (power$DateTime_GMT>=ymd_hms(20090813000100) & power$DateTime_GMT<=ymd_hms(20090813235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100112000100) & power$DateTime_GMT<=ymd_hms(20100114235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100320000100) & power$DateTime_GMT<=ymd_hms(20100321235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100817000100) & power$DateTime_GMT<=ymd_hms(20100822235900)) |
                   (power$DateTime_GMT>=ymd_hms(20100925000100) & power$DateTime_GMT<=ymd_hms(20100928235900)))) 
power[indexV,] <- power[indexV,] %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
power <- na.locf(power, fromLast = FALSE) 

#change units active and reactive to Wh
power[,1:2] <- power[ ,1:2]*(1000/60)
#new variable with energy consumption that is not detected by the sub-meters
power <- mutate(power,Other = (Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3))
#data manipulation for the monthly summary
power_monthly <- power %>%
  group_by(month(DateTime_GMT),year(DateTime_GMT)) %>%
  summarise(active=sum(Global_active_power)/1000,reactive=sum(Global_reactive_power)/1000,
            sub_metering_1=sum(Sub_metering_1)/1000,sub_metering_2=sum(Sub_metering_2)/1000,
            sub_metering_3=sum(Sub_metering_3)/1000, other=sum(Other)/1000) %>%
  arrange(`year(DateTime_GMT)`,`month(DateTime_GMT)`)
#Forecast active power
power_monthly_TS <- ts(power_monthly[2:47,3:8], frequency=12, start=c(2007,1))
active_monthly_arima <- arima(power_monthly_TS[,1], order=c(0,0,0), seasonal=c(1,1,0)) # fit an ARIMA(0,0,0)(1,1,0) model
active_monthly_arima_forecast <- forecast(active_monthly_arima,h=12) #forecast next year

#----------------------user interface-------------------------------------------
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "User Energy Consumption")
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
sidebarMenu(
menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
)
)

#Select month and year on top
frow0 <- fluidRow(
#  column (width=3, h1("Monthly summary")),
column (width=2, selectInput(
inputId = "selectmonth", label="Select month",
list("January"=1,"February"=2,"March"=3,"April"=4,"May"=5, "June"=6,
"July"=7, "August"=8, "September"=9,"October"=10,"November"=11,"December"=12),
selected = tail(power_monthly$`month(DateTime_GMT)`,1)
)),
column (width=2, selectInput(
inputId = "selectyear", label="Select year",
list(2006,2007,2008,2009,2010),
selected= tail(power_monthly$`year(DateTime_GMT)`,1)
))
)

#gauge plot and value boxes on top
frow1 <- fluidRow( #aligned boxes on top
box(
title="Total energy consumed",
#status = "primary", #determines background color
background="navy",
solidHeader = TRUE,
width = 4,
height="170px",
gaugeOutput("gaugeKwh")
),
tags$head(tags$style(HTML(".small-box {height: 170px}"))),
valueBoxOutput("cost"),
valueBoxOutput("co2")
#  valueBoxOutput("peak",width=NULL)
)

frow2 <- fluidRow( #aligned plots
box(
title="Energy consumption",
status = "primary", #determines background color
solidHeader = TRUE,
collapsible = TRUE,
width = 8,
plotOutput("consumption",height="300px")
),
box(
title="Energy Consumption",
status="primary",
solidHeader = TRUE,
collapsible = TRUE,
#  background =  "maroon",
width = 4,
plotOutput("meters", height = "300px")
)
)

#combine the two fluid rows to make the body
body <- dashboardBody(h1("Monthly summary"),frow0,frow1,frow2)
ui <- dashboardPage(
title = "Energy consumption", #title of the browser page
header, sidebar, body,
skin="red"
)

server <- function(input, output) {
#data manipulation for the monthly summary
selectedMonth <- reactive({ power_monthly %>%
filter((`month(DateTime_GMT)`== input$selectmonth) & (`year(DateTime_GMT)`== input$selectyear))
})

output$gaugeKwh = renderGauge({  #if to change the previous months
kwh_selectedMonth <- selectedMonth()
gauge(round(kwh_selectedMonth$active),
symbol="kWh",
min = 0,
max = round( active_monthly_arima_forecast$mean[1]),
sectors = gaugeSectors(success = c(0,0.3*(active_monthly_arima_forecast$mean[1])),
warning = c(0.3*(active_monthly_arima_forecast$mean[1]),0.6*(active_monthly_arima_forecast$mean[1])),
danger =  c(0.6*(active_monthly_arima_forecast$mean[1]),active_monthly_arima_forecast$mean[1])))
})

output$cost <- renderValueBox({
cost_selectedMonth <- selectedMonth()
cost_selectedMonth <- cost_selectedMonth$active * 0.0135
valueBox(
round(cost_selectedMonth,2),
"Expected cost",
icon=icon("euro", lib="glyphicon"),
color = "yellow"
)
})

output$co2 <- renderValueBox({
co2_selectedMonth <- selectedMonth()
co2_selectedMonth <- co2_selectedMonth$active * 80/1000 #in 2010 in france 80g/kWh
valueBox(
paste(round(co2_selectedMonth,2),"Kg"),
"Expected CO2 emission",
icon=icon("fas fa-leaf"),
color = "green"
)
})

output$meters <- renderPlot({
meters_selectedMonth <- selectedMonth()
#Gather the sub-meters to help with the further visualization
meters_selectedMonth <- meters_selectedMonth %>% gather(Meter, kWh, "sub_metering_1", "sub_metering_2","sub_metering_3","other")
ggplot(data=meters_selectedMonth,
aes(x=reorder(Meter,kWh),y=kWh,fill=factor(Meter)))+
geom_bar(stat="identity")+
coord_flip() +
labs(y='kWh') +
theme(legend.position = "none", axis.title.y=element_blank(), axis.title.x = element_text(face="bold",size=10),
axis.text.x = element_text(size=10), axis.text.y = element_text(face="bold",size=10), axis.ticks.y = element_blank(),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
scale_x_discrete(labels=c("other"="Other", "sub_metering_1"="Kitchen","sub_metering_2"="Laundry","sub_metering_3"="Boiler+AC"))
#change color background and add value to bars, y labels
})
}
shinyApp(ui = ui, server = server)

