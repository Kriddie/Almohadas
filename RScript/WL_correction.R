#script to correct water level data for barometric pressure

library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

#read in water level data
df_WL <- read.csv(here::here("FieldData/WaterTable_CayambeCoca_2023-10-23.csv"),header = TRUE,skip=11)[c(1,2,4,5)]

colnames(df_WL) <- c("Date","Time","WL_kpa","WLTemp_c")
df_WL$DateTime <- as.POSIXct(paste(df_WL$Date, df_WL$Time), format="%m/%d/%Y %I:%M:%S %p", tz="UTC")

#use this to clean data if necessary
fig1 <- plot_ly(data = df_WL, x = ~DateTime, y = ~WL_kpa)
fig1

#clean
df_WL <- df_WL%>%
  filter(DateTime <= as.POSIXct("2022-12-20 09:30:00", tz="UTC")|
        DateTime > as.POSIXct("2022-12-20 11:00:00", tz="UTC"))%>%
  filter(DateTime <= as.POSIXct("2023-05-24 11:30:00", tz="UTC")|
           DateTime > as.POSIXct("2023-05-24 12:45:00", tz="UTC"))%>%
  filter(DateTime <= as.POSIXct("2023-08-25 13:00:00", tz="UTC")|
           DateTime > as.POSIXct("2023-08-25 14:15:00", tz="UTC"))%>%
  filter(DateTime <= as.POSIXct("2023-10-31 11:15:00", tz="UTC")|
           DateTime > as.POSIXct("2023-10-31 14:45:00", tz="UTC"))

#read in Barometric data
df_Baro <- read.csv(here::here("FieldData/Barometric_CCGavilan_2023-08-25"),header = TRUE)[2:4]
colnames(df_Baro) <- c("DateTime","AirPressure_kpa","AirTemp_c")
df_Baro$DateTime <- as.POSIXct(df_Baro$DateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

fig2 <- plot_ly(data = df_Baro, x = ~DateTime, y = ~AirPressure_kpa)
fig2

#join data frames
df_All <- inner_join(df_Baro,df_WL,join_by("DateTime"))

#calc water level in cm
  #1 kilopascals = 0.10199773339984 meters of head 
  # 100 cm in a meter
df_All$WL_cm <- (df_All$WL_kpa - df_All$AirPressure_kpa) * .10199773339984 * 100

#select desired columns
df_All <- df_All[,c("DateTime","AirPressure_kpa","AirTemp_c","WL_cm","WLTemp_c")]


#check data by plotting
p1 <- ggplot(df_All, aes(x=DateTime)) +
  geom_point( aes(y=WL_cm),color="brown")
p1

#write out
#write.csv(df_All, here::here("FieldData/WL_parqueadero_corrected_2023-08-25.csv"))
