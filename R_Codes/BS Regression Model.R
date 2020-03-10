#Exploring The Dataset

library(tidyverse)
library(dplyr)
#Maps Libraries
library(leaflet)
library(leaflet.extras)

#Color libraries
library(RColorBrewer)

#Date library
library(lubridate)

#TimeSeries forecasting libraries
library(imputeTS)
library(forecast)

#DataTable libraries
library(DT)

#Heat Map Libraries
library(r.jive)

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

getwd()
setwd("/Users/Dell/Downloads")
BikeTrips = read_csv("BSS.csv")
BikeStations = read_csv("BS_Stations.csv")
nrow(BikeTrips)
datatable(head(BikeTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

summary(BikeTrips)

#Most Bike from checkout station#
BikeTrips %>%
  filter(!is.na(`Checkout Kiosk`)) %>%
  group_by(`Checkout Kiosk`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(start_station_name = reorder(`Checkout Kiosk`,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = start_station_name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = start_station_name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Station Name', 
       y = 'Count', 
       title = 'Station Name and Count') +
  coord_flip() + 
  theme_bw()
  
#MOst Bike at returning station#
  BikeTrips %>%
  filter(!is.na(`Return Kiosk`)) %>%
  group_by(`Return Kiosk`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(end_station_name = reorder(`Return Kiosk`,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = end_station_name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = end_station_name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Station Name', 
       y = 'Count', 
       title = 'Station Name and Count') +
  coord_flip() + 
  theme_bw()

GetMostPopularRoutes = function(BikeTrips)
{
  return (
    
    BikeTrips %>%
      filter(!is.na(`Return Kiosk`)) %>%
      filter(!is.na(`Checkout Kiosk`)) %>%
      group_by(`Checkout Kiosk`,`Return Kiosk`) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(6)
    
  )
  
}

datatable(GetMostPopularRoutes(BikeTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#Map Of Austin Bike Station#
center_lon = median(BikeStations$longitude,na.rm = TRUE)
center_lat = median(BikeStations$latitude,na.rm = TRUE)

factpal <- colorFactor(c("yellow","blue","red","cyan"), 
                       BikeStations$status)


leaflet(BikeStations) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
             color = ~factpal(status))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 15) %>%
  
  addLegend("bottomright", pal = factpal, values = ~status,
            title = "Bike Stations Status",
            opacity = 1)

#Busiest Months#
BikeTrips %>%
  filter(!is.na(Month)) %>%
  group_by(Month) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Month = reorder(Month,Count)) %>%
  
  ggplot(aes(x = Month,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = Month, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Month', 
       y = 'Count', 
       title = 'Month and Count') +
  coord_flip() + 
  theme_bw()


library(tidyr)
BikeTrips %>% drop_na(Day)

#Busy Days
GetTop10BusyDays = function(BikeTrips)
{
  
  BikeTrips = BikeTrips %>%
    mutate(day = day(mdy_hms(BikeTrips$start_time)))
  
  BikeTrips %>%
    filter(!is.na(day)) %>%
    group_by(day) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(day = reorder(day,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = day,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = day, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'day', 
         y = 'Count', 
         title = 'Day and Count') +
    coord_flip() + 
    theme_bw()
}

GetTop10BusyDays(BikeTrips)

#least Busy days
GetLeast10BusyDays = function(BikeTrips)
{
  
  BikeTrips = BikeTrips %>%
    mutate(day = day(mdy_hms(BikeTrips$start_time)))
  
  BikeTrips %>%
    filter(!is.na(day)) %>%
    group_by(day) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(day = reorder(day,Count)) %>%
    tail(10) %>%
    
    ggplot(aes(x = day,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = day, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'day', 
         y = 'Count', 
         title = 'day and Count') +
    coord_flip() + 
    theme_bw()
}

GetLeast10BusyDays(BikeTrips)

#Busy hours
BikeTrips$hour = hour(BikeTrips$Checkout Time)

GetTop10BusyHours = function(BikeTrips)
{
  BikeTrips %>%
    filter(!is.na(hour)) %>%
    group_by(hour) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(hour = reorder(hour,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = hour,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor2) +
    geom_text(aes(x = hour, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'hour', 
         y = 'Count', 
         title = 'hour and Count') +
    coord_flip() + 
    theme_bw()
  
}

GetTop10BusyHours(BikeTrips)

#Type of Subscriber
GetTypesOfMembershipType = function(BikeTrips)
{
  BikeTrips %>%
    filter(!is.na(`Membership Type`)) %>%
    group_by(`Membership Type`) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(Membership_Type = reorder(`Membership Type`,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = `Membership Type`,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = Membership_Type, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Membership Type', 
         y = 'Count', 
         title = 'Membership Type and Count') +
    coord_flip() + 
    theme_bw()
  
}

GetTypesOfMembershipType(BikeTrips)

# Duration of Trips#
summary(BikeTrips$`Trip Duration Minutes`)

BikeTrips %>%
  filter(`Trip Duration Minutes` <=22) %>%
  ggplot(aes(x = `Trip Duration Minutes`) )+
  geom_histogram(fill = fillColor2) +
  labs(x = 'Minutes' ,y = 'Count', title = paste("Distribution of", "duration")) +
  theme_bw()

# Time Series
BikeStationMostTrips = BikeTrips %>%
  filter(`Checkout Kiosk ID` == 3798) %>%
  filter(!is.na(Year)) %>%
  filter(!is.na(Month)) %>%
  group_by(Year,Month) %>%
  summarise(Count = n()) %>%
  arrange(Year,Month)

dataRow1 = data.frame(Year=2018,Month= 2,Count=NA)
dataRow2 = data.frame(Year=2018,Month= 10,Count=NA)

BikeStationMostTrips = as.data.frame(BikeStationMostTrips)
BikeStationMostTrips = rbind(BikeStationMostTrips,dataRow1)
BikeStationMostTrips = rbind(BikeStationMostTrips,dataRow2)

BikeStationMostTrips = BikeStationMostTrips %>% arrange(Year,Month)

tsBikeStationMostTrips = ts(BikeStationMostTrips)

datatable((tsBikeStationMostTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

tsBikeStationMostTrips = na_interpolation(tsBikeStationMostTrips,option="stine")

                       #Arima Model#
fit <- auto.arima(tsBikeStationMostTrips[1:7,3])
preds = forecast(fit, h = 6)
preds %>% autoplot(include=7) +theme_bw()
#forecast results
predictions = as.numeric(preds$mean)
cat("\n","The predictions are  ",predictions)
error = sqrt( mean( (tsBikeStationMostTrips[8:10,3] - predictions)^2))
cat("\n","The RMSE is ", error)

                #Linear Regression Model#
fit = tslm(Count ~ Year + Month,data = head(tsBikeStationMostTrips,7))
newdata = as.data.frame(tsBikeStationMostTrips[1:7,1:2])
preds = forecast(fit, newdata = newdata)
preds %>% autoplot(include=7) +theme_bw()
#forecast results
predictions = as.numeric(preds$mean)
cat("\n","The predictions are  ",predictions)
error = sqrt( mean( (tsBikeStationMostTrips[8:10,3] - predictions)^2))
cat("\n","The RMSE is ", error)

#No of Trips During Festival#
BikeTripsFestival = BikeTrips %>%
  filter(`start_time` >= mdy('3-9-2018')) %>%
  filter(`start_time` < mdy('3-18-2018'))
nrow(BikeTripsFestival)

ShowHeatMaps = function(BikeTripsFestival)
{
  BikeTripsFestival$StartDate = as.Date(BikeTripsFestival$start_time)
  
  hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
  
  BikeTripsFestival %>%
    group_by(StartDate,hour) %>%
    summarise(Count = n()) %>%
    
    ggplot(aes(x = StartDate, y = hour)) +
    geom_tile(aes(fill = Count)) + 
    geom_text(aes(label = Count)) +
    scale_fill_gradientn(colours = hm.palette(100)) +
    theme_bw()
}

ShowHeatMaps(BikeTripsFestival)


        
datatable(GetMostPopularRoutes(BikeTripsFestival), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
