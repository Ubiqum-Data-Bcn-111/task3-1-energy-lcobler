#Shiny dashboard for energy consumption
#Lara Cobler Moncunill
#21 December 2018

library(flexdashboard) #for gauge plot
library(shiny)
require(shinydashboard)
library(shinyWidgets) #fancy widgets
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
#data manipulation for the daily summary
power_dayly <- power %>%
  group_by(day(DateTime_GMT),month(DateTime_GMT),year(DateTime_GMT)) %>%
  summarise(Total=sum(Global_active_power)/1000,Reactive=sum(Global_reactive_power)/1000,
            Kitchen=sum(Sub_metering_1)/1000,"Laundry room"=sum(Sub_metering_2)/1000,
            "Boiler+AC"=sum(Sub_metering_3)/1000, Other=sum(Other)/1000) %>%
  gather(Meter, kWh, "Kitchen", "Laundry room","Boiler+AC","Other","Total","Reactive") %>%
  arrange(`year(DateTime_GMT)`,`month(DateTime_GMT)`,`day(DateTime_GMT)`)
#Forecast active power
power_monthly_TS <- ts(power_monthly[2:47,3:8], frequency=12, start=c(2007,1))
active_monthly_arima <- arima(power_monthly_TS[,1], order=c(0,0,0), seasonal=c(1,1,0)) # fit an ARIMA(0,0,0)(1,1,0) model
active_monthly_arima_forecast <- forecast(active_monthly_arima,h=12) #forecast next year
#isolate refrigerator
library(imputeTS) #must be colled after fixing NAs to avoid masking
power_fridge <- power[power$Sub_metering_2<=2, c(6,8)] #remove values >2
allDates <- seq(min(power_fridge$DateTime_GMT), max(power_fridge$DateTime_GMT),by="mins") #include removed dates
power_fridge <- merge(x=data.frame(DateTime_GMT=allDates), y=power_fridge, all.x=TRUE)
fridge_ts <- ts(power_fridge[,2],frequency=525600) #time series to impute missing values
fridge_ts <- na.seadec(fridge_ts,algorithm="interpolation") #impute NAs by removing seasonality and adding back
pred_fridge <- as.matrix(fridge_ts) #refrigerator values with imputed NAs
power <- cbind(power, pred_fridge) #add column with refrigerator values
#change names for dates plot
power_dates <- power%>% 
  setNames(list("Total", "Reactive", "Voltage", "Intensity","Kitchen","Laundry room","Boiler+AC","DateTime_GMT","DateTime_TC","Other","Refrigerator")) %>%
  gather(Meter, kWh, "Kitchen", "Laundry room","Boiler+AC","Other","Total","Reactive","Refrigerator")

#----------------------user interface-------------------------------------------
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "User Dashboard")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Energy consumption", tabName= "plot", icon = icon("bar-chart-o"))
    )
  )

#Select month and year on top
frow0 <- fluidRow(
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
#gauge plot and value boxes in the middle
frow1 <- fluidRow( #aligned boxes on top
  box(
    title="Total energy consumed",
    background="navy",
    solidHeader = TRUE,
    width = 3,
    height="170px",
    gaugeOutput("gaugeKwh")
    ),
  tags$head(tags$style(HTML(".small-box {height: 170px}"))), #size value box equal to gauge box (170px)
  valueBoxOutput("cost", width=3),
  valueBoxOutput("co2", width=3),
  valueBoxOutput("peak", width=3)
  )
#plots at the bottom
frow2 <- fluidRow( #aligned plots
  box(
    title="Energy consumption",
    status = "primary", #determines background color
    solidHeader = TRUE,
    width = 8,
    checkboxGroupButtons(
      inputId = "selectMeter", #label = "Select a meter :", 
      choices = c("Kitchen", "Laundry room", "Boiler+AC", "Other", "Total", "Reactive"), 
      justified = TRUE, status = "primary",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
      selected="Total"
    ),
    plotOutput("consumption",height="300px")
    ),
  box(
    title="Energy Consumption",
    status="primary",
    solidHeader = TRUE,
    width = 4, height="414px",
    plotOutput("meters", height = "350px")
    )
  )
frow3 <- fluidRow( 
  box(
    title="Energy consumption",status="success",solidHeader = TRUE,
    width= 9,
    plotOutput("usage",height=400)),
  box(
    title="Controls",background = "purple",solidHeader = TRUE,
    width=3, height ="462px",
    dateRangeInput(inputId = "dates", label = "Date range", start="2010-11-23", end="2010-11-26"),
    checkboxGroupInput("checkMeters", label = "Select meters: ", 
                       choices = c("Kitchen", "Laundry room", "Boiler+AC", "Other", "Total", "Reactive","Refrigerator"),
                       selected = "Total")
  )
)
#combine the fluid rows to make the body
body <- dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size  of tab items to 20
  ),
  tabItems(
    tabItem(tabName = "dashboard", h1("Monthly summary"),frow0,frow1,frow2),
    tabItem(tabName = "plot", frow3)
  ))
ui <- dashboardPage(
  title = "Energy consumption", #title of the browser page
  header, sidebar, body,
  skin="red"
  )

#---------------------------------------------server-----------------------------------------------
server <- function(input, output) {
  
#data manipulation for the monthly summary
  selectedMonth <- reactive({ power_monthly %>%
      filter((`month(DateTime_GMT)`== input$selectmonth) & (`year(DateTime_GMT)`== input$selectyear))
    })

#data manioulation for monthly summary dayly plot
  selectedMonth_dayly <- reactive ({
    power_dayly %>% filter((`month(DateTime_GMT)`== input$selectmonth) & (`year(DateTime_GMT)`== input$selectyear) &
                             (Meter %in% input$selectMeter))
  })
  
  selectedDates <- reactive ({
    power_dates %>% filter((DateTime_TC >= input$dates[1] & DateTime_TC <= (input$dates[2]+days(1))) &
                             (Meter %in% input$checkMeters))
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
    round(cost_selectedMonth,2), #large number
    "Monthly calculated cost", #subtitle
    icon=icon("euro", lib="glyphicon"), 
    color = "yellow"
    )
  })

output$co2 <- renderValueBox({
  co2_selectedMonth <- selectedMonth()
  co2_selectedMonth <- co2_selectedMonth$active * 80/1000 #in 2010 in france 80g/kWh
  valueBox(
    paste(round(co2_selectedMonth,2),"Kg"),
    "Monthly calculated CO2 emission",
    icon=icon("fas fa-leaf"),
    color = "green"
    )
  })

output$peak <- renderValueBox({
  peak_selectedMonth <- selectedMonth()
  if (peak_selectedMonth$`month(DateTime_GMT)` >= 4 & peak_selectedMonth$`month(DateTime_GMT)` <= 9){
    peak_season = "11am - 3pm"
  } else {
    peak_season = "6pm - 9pm"
  }
  valueBox(
    peak_season,
    "Peak electricity demand",
    icon=icon("time", lib="glyphicon"),
    color = "purple"
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
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
    scale_x_discrete(labels=c("other"="Other", "sub_metering_1"="Kitchen","sub_metering_2"="Laundry","sub_metering_3"="Boiler+AC"))
  })

output$consumption <- renderPlot({
  meters_dayly <- selectedMonth_dayly()
  ggplot(data=meters_dayly, aes(x=factor(`day(DateTime_GMT)`),kWh,group=Meter,color=Meter)) +
    labs(x='Day', y='kWh') +
    geom_line(aes(color=Meter),size=1) +
    geom_line(aes(color=Meter),size=1) +
    geom_line(aes(color=Meter),size=1) +
    geom_line(aes(color=Meter),size=1) +
    geom_line(aes(color=Meter),size=1) +
    geom_line(aes(color=Meter),size=1) +
    scale_colour_manual(values = c("Total" = "steelblue", "Reactive" = "#F564E3", "Kitchen"="#7CAE00","Laundry room"="#00BFC4",
                                   "Boiler+AC"="#C77CFF","Other"="#F8766D"))+ #ASsign colors to each variable
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.line = element_line(colour = "steelblue", size = 1, linetype = "solid"))
})

output$usage <- renderPlot ({
  selectedDates <- selectedDates()
  #date range 
  #3 grouping depending on the selected days
  if (difftime(input$dates[2], input$dates[1], "days") <= 15){
    selectedDates %>%
      group_by(hour(DateTime_TC),day(DateTime_TC), Meter) %>%
      summarise(sum=sum(kWh)/1000) %>%
      ggplot( aes(x=factor(`hour(DateTime_TC)`),sum,group=Meter,color=Meter)) +
      labs(x='Hour of the day', y='kWh') +
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      scale_colour_manual(values = c("Total" = "steelblue", "Reactive" = "#F564E3", "Kitchen"="#7CAE00","Laundry room"="#00BFC4",
                                     "Boiler+AC"="#C77CFF","Other"="#F8766D","Refrigerator"="Orange"))+
      facet_wrap(~ factor(`day(DateTime_TC)`))+
      theme (strip.text.x = element_text(size=12, face="bold"),
             strip.background = element_rect (fill="#00B250"))
  } else if (difftime(input$dates[2], input$dates[1], "days") <= 270) {
    selectedDates %>%
      group_by(day(DateTime_TC),month(DateTime_TC), Meter) %>%
      summarise(sum=sum(kWh)/1000) %>%
      ggplot( aes(x=factor(`day(DateTime_TC)`),sum,group=Meter,color=Meter)) +
      labs(x='Day of the month', y='kWh') +
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      scale_colour_manual(values = c("Total" = "steelblue", "Reactive" = "#F564E3", "Kitchen"="#7CAE00","Laundry room"="#00BFC4",
                                     "Boiler+AC"="#C77CFF","Other"="#F8766D","Refrigerator"="Orange"))+
      facet_wrap(~ factor(`month(DateTime_TC)`))+
      theme (strip.text.x = element_text(size=12, face="bold"),
             strip.background = element_rect (fill="#00B250"))
  } else {
    selectedDates %>%
      group_by(month(DateTime_TC),year(DateTime_TC), Meter) %>%
      summarise(sum=sum(kWh)/1000) %>%
      ggplot( aes(x=factor(`month(DateTime_TC)`),sum,group=Meter,color=Meter)) +
      labs(x='Month', y='kWh') +
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      geom_line(aes(color=Meter),size=1)+
      scale_colour_manual(values = c("Total" = "steelblue", "Reactive" = "#F564E3", "Kitchen"="#7CAE00","Laundry room"="#00BFC4",
                                     "Boiler+AC"="#C77CFF","Other"="#F8766D","Refrigerator"="Orange"))+
      facet_wrap(~ factor(`year(DateTime_TC)`))+
      theme (strip.text.x = element_text(size=12, face="bold"),
             strip.background = element_rect (fill="#00B250"))
 }
})


}
shinyApp(ui = ui, server = server)


