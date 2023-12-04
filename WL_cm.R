#Graphing temp, water level by time 
#libraries I need 
library(lubridate)     
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)

WL_parqueadero_csv$DateTime <- as.POSIXct(WL_parqueadero_csv$DateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT-5")

ggplot(data = WL_parqueadero_csv, aes(x = DateTime, y = WL_cm, group = WL_cm, color = WL_cm)) +
    geom_point() +
    labs(x = "Date", y = "Water Level") +
    ggtitle("Water Level") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(date_breaks = "months", labels = scales::date_format("%Y-%m-%d %H:%M:%S")) 
#I don't know why the dates are not well displayed in the x-axis. 
P <- ggplot(data = WL_parqueadero_csv, aes(x = DateTime, y = WL_cm)) + 
  geom_point(size=1)#With this code I have the dates divide by months but It doesn't show the dates from 2022



  
