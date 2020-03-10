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
BikeTrips = read_csv("BS_Trips.csv")
BikeStations = read_csv("BS_Stations.csv")
nrow(BikeTrips)
datatable(head(BikeTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

BikeTrips %>%
  filter(!is.na(start_station_name)) %>%
  group_by(start_station_name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(start_station_name = reorder(start_station_name,Count)) %>%
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
  
  BikeTrips %>%
  filter(!is.na(end_station_name)) %>%
  group_by(end_station_name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(end_station_name = reorder(end_station_name,Count)) %>%
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
      filter(!is.na(end_station_name)) %>%
      filter(!is.na(start_station_name)) %>%
      group_by(start_station_name,end_station_name) %>%
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
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(month = reorder(month,Count)) %>%
  
  ggplot(aes(x = month,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = month, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Month', 
       y = 'Count', 
       title = 'Month and Count') +
  coord_flip() + 
  theme_bw()

#Busy Days
GetTop10BusyDays = function(BikeTrips)
{
  
  BikeTrips = BikeTrips %>%
    mutate(day = day(ymd_hms(BikeTrips$start_time)))
  
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
         title = 'day and Count') +
    coord_flip() + 
    theme_bw()
}

GetTop10BusyDays(BikeTrips)

#least Busy days
GetLeast10BusyDays = function(BikeTrips)
{
  
  BikeTrips = BikeTrips %>%
    mutate(day = day(ymd_hms(BikeTrips$start_time)))
  
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
BikeTrips$hour = hour(BikeTrips$checkout_time)

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
GetTypesOfSubscribers = function(BikeTrips)
{
  BikeTrips %>%
    filter(!is.na(subscriber_type)) %>%
    group_by(subscriber_type) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(subscriber_type = reorder(subscriber_type,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = subscriber_type,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor) +
    geom_text(aes(x = subscriber_type, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'subscriber_type', 
         y = 'Count', 
         title = 'subscriber_type and Count') +
    coord_flip() + 
    theme_bw()
  
}

GetTypesOfSubscribers(BikeTrips)

# Duration of Trips#
summary(BikeTrips$duration_minutes)

BikeTrips %>%
  filter(duration_minutes <=100) %>%
  ggplot(aes(x = duration_minutes) )+
  geom_histogram(fill = fillColor2) +
  labs(x = 'Minutes' ,y = 'Count', title = paste("Distribution of", "duration")) +
  theme_bw()

BikeStationMostTrips = BikeTrips %>%
  filter(start_station_id == 2575) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  arrange(year,month)

dataRow1 = data.frame(year=2016,month= 4,Count=NA)
dataRow2 = data.frame(year=2016,month= 12,Count=NA)

BikeStationMostTrips = as.data.frame(BikeStationMostTrips)
BikeStationMostTrips = rbind(BikeStationMostTrips,dataRow1)
BikeStationMostTrips = rbind(BikeStationMostTrips,dataRow2)

BikeStationMostTrips = BikeStationMostTrips %>% arrange(year,month)

tsBikeStationMostTrips = ts(BikeStationMostTrips)

datatable((tsBikeStationMostTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

tsBikeStationMostTrips = na_interpolation(tsBikeStationMostTrips,option="stine")

                       #Arima Model#
fit <- auto.arima(tsBikeStationMostTrips[1:34,3])
preds = forecast(fit, h = 6)
preds %>% autoplot(include=34) +theme_bw()
#forecast results
predictions = as.numeric(preds$mean)
cat("\n","The predictions are  ",predictions)
error = sqrt( mean( (tsBikeStationMostTrips[35:40,3] - predictions)^2))
cat("\n","The RMSE is ", error)

                #Linear Regression Model#
fit = tslm(Count ~ year + month,data = head(tsBikeStationMostTrips,34))
newdata = as.data.frame(tsBikeStationMostTrips[35:40,1:2])
preds = forecast(fit, newdata = newdata)
preds %>% autoplot(include=34) +theme_bw()
#forecast results
predictions = as.numeric(preds$mean)
cat("\n","The predictions are  ",predictions)
error = sqrt( mean( (tsBikeStationMostTrips[35:40,3] - predictions)^2))
cat("\n","The RMSE is ", error)

#No of Trips During Festival#
BikeTripsFestival = BikeTrips %>%
  filter(start_time >= ymd('2016-3-11')) %>%
  filter(start_time < ymd('2016-3-21'))
nrow(BikeTripsFestival)

heatmaps(BikeTripsFestival)

datatable(GetMostPopularRoutes(BikeTripsFestival), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
