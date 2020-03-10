#Exploring The Dataset

library(tidyverse)
library(dplyr)
library(tidyr)

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
DVTrips = read_csv("DV_Trips.csv")
nrow(DVTrips)
datatable(head(DVTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
DVTrips%>% drop_na(`Start Latitude`)
DVTrips%>% drop_na(`Start Longitude`)

#Busiest Months#
DVTrips %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Month = reorder(month,Count)) %>%
  
  ggplot(aes(x = month,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = month, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=0.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Month', 
       y = 'Count', 
       title = 'Month and Count') +
  coord_flip() + 
  theme_bw()


GetTop10BusyDays = function(DVTrips)
{
DVTrips = DVTrips %>%
  mutate(day = day(dmy_hms(DVTrips$Start Time)))

DVTrips %>%
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

GetTop10BusyDays(DVTrips)

#Duration Of Trips
summary(DVTrips$duration_minutes)

DVTrips %>%
  filter(duration_minutes <=100) %>%
  ggplot(aes(x = duration_minutes) )+
  geom_histogram(fill = fillColor2) +
  labs(x = 'Minutes' ,y = 'Count', title = paste("Distribution of", "duration")) +
  theme_bw()

#Most Bike from Origin Cell#
DVTrips %>%
  filter(!is.na(`Origin Cell ID`)) %>%
  group_by(`Origin Cell ID`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(start_station_name = reorder(`Origin Cell ID`,Count)) %>%
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

#Most Bike going to Destination Cell#
DVTrips %>%
  filter(!is.na(`Destination Cell ID`)) %>%
  group_by(`Destination Cell ID`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(end_station_name = reorder(`Destination Cell ID`,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = end_station_name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = end_station_name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Station Name', 
       y = 'Count', 
       title = ' End Station Name and Count') +
  coord_flip() + 
  theme_bw()


#Trip Duration For Origin cell ID 14389
DVTrips %>%
  filter(`Origin Cell ID` == 14389) %>%
  filter(!is.na(Year)) %>%
  filter(!is.na(month)) %>%
  group_by(Year,month) %>%
  summarise(Count = n()) %>%
  mutate(YearMonth = paste0(Year,"_",month)) %>%
  arrange(Year,month) %>%
  ungroup() %>%
  
  ggplot(aes(x=YearMonth,y=Count)) +
  geom_bar(stat = 'identity',colour="white", fill = fillColor) +
  labs(x = 'Time', y = 'Count',title = 'Trend of Bike Rides') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Time series Model
DVMostTrips = DVTrips %>%
  filter(`Origin Cell ID` == 14389) %>%
  filter(!is.na(Year)) %>%
  filter(!is.na(month)) %>%
  group_by(Year,month) %>%
  summarise(Count = n()) %>%
  arrange(Year,month)

dataRow1 = data.frame(Year=2018,month= 7,Count=NA)
dataRow2 = data.frame(Year=2018,month= 12,Count=NA)

DVMostTrips = as.data.frame(DVMostTrips)
DVMostTrips = rbind(DVMostTrips,dataRow1)
DVMostTrips = rbind(DVMostTrips,dataRow2)

DVMostTrips = DVMostTrips %>% arrange(Year,month)

tsDVMostTrips = ts(DVMostTrips)

datatable((tsDVMostTrips), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

tsDVMostTrips = na_interpolation(tsDVMostTrips,option="stine")

                          # Arima Model#
fit <- auto.arima(tsDVMostTrips[1:4,3])
preds = forecast(fit, h = 6)
preds %>% autoplot(include=4) +theme_bw()
#forecast results
predictions = as.numeric(preds$mean)
cat("\n","The predictions are  ",predictions)
error = sqrt( mean( (tsDVMostTrips[5:7,3] - predictions)^2))
cat("\n","The RMSE is ", error)

#Linear Regression Model#
fit = tslm(Count ~ Year + month,data = head(tsDVMostTrips,4))
newdata = as.data.frame(tsDVMostTrips[5:7,1:2])
preds = forecast(fit, newdata = newdata)
preds %>% autoplot(include=4) +theme_bw()
#forecast results
predictions = as.numeric(preds$mean)
cat("\n","The predictions are  ",predictions)
error = sqrt( mean( (tsDVMostTrips[5:8,3] - predictions)^2))
cat("\n","The RMSE is ", error)

#No of Trips During Festival#
DVTripsFestival = DVTrips %>%
  filter(`Start Time` >= mdy('3/9/2018')) %>%
  filter(`Start Time` < mdy('3/18/2018'))
nrow(DVTripsFestival)

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