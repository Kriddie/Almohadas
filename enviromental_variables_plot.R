#Graphing temp, water level by time 
#libraries I need 
library(lubridate)     
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(here)
#WATER TABLE PLOT
WL_parqueadero_csv <- read.csv("FieldData/WL_parqueadero_corrected_2023-08-25.csv")
View(WL_parqueadero_csv)
  
WL_parqueadero_csv$DateTime <- as.POSIXct(WL_parqueadero_csv$DateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")


ggplot(data = WL_parqueadero_csv, aes(x = DateTime, y = WL_cm, group = WL_cm, color = WL_cm)) +
    geom_point() +
    labs(x = "Date", y = "Water Table") +
    ggtitle("Water Level") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) #+



#x-axis divided by month and year 
library(scales)
ggplot(data = WL_parqueadero_csv, aes(x = DateTime, y = WL_cm, color = WL_cm)) +
  geom_point() +
  labs(x = "Date", y = "Water Table") +
  ggtitle("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(min(WL_parqueadero_csv$WL_cm) - 1, max(WL_parqueadero_csv$WL_cm) + 1) +
  # Set breaks and labels for the x-axis
  scale_x_datetime(breaks = seq(from = as.POSIXct("2022-01-01"), to = as.POSIXct("2023-08-01"), by = "1 month"),
                   labels = date_format("%b-%Y"))
#MOISTURE INSIDE THE PLANT
logger9710 <- read_excel(
  "~/Almohada project (compu)/HOBO data/hobo  data_4-07-2023/21519710_julio.xlsx",
  col_types = c("numeric", "date", "numeric", "numeric")
)
logger9710_1 <- read_excel(
  "~/Almohada project (compu)/HOBO data/hobo data_28-07-2023/logger_9710071.xlsx",
  col_types = c("numeric", "date", "numeric", "numeric")
)

logger9710_2 <- read_excel(
  "~/Almohada project (compu)/HOBO data/hobo data_12-10-2023/logger 9710.xlsx",
  col_types = c("numeric", "date", "numeric", "numeric")
)
logger9710_3 <- read_excel(
  "~/Almohada project (compu)/HOBO data/hobo data_20-07-2023/logger_971007.xlsx",
  col_types = c("numeric", "date", "numeric", "numeric")
)
logger9710_4 <- read_excel(
  "~/Almohada project (compu)/HOBO data/hobo data_25-08-2023/logger 9710.xlsx",
  col_types = c("numeric", "date", "numeric", "numeric")
)
logger9710_4 <- read_excel(
  "~/Almohada project (compu)/HOBO data/hobo data_30-09-2023/logger 9710.xlsx",
  col_types = c("numeric", "date", "numeric", "numeric")
)
View(logger9710)

#change columns names 
colnames(logger9710) <- c("null", "DateTime", "water_m3_m3", "temp")

colnames(logger9710_1) <- c("null", "DateTime", "water_m3_m3", "temp")

colnames(logger9710_2) <- c("null", "DateTime", "water_m3_m3", "temp")

colnames(logger9710_3) <- c("null", "DateTime", "water_m3_m3", "temp")

colnames(logger9710_4) <- c("null", "DateTime", "water_m3_m3", "temp")
#bind all the documents to one dataframe 
logger_moisture <- rbind(logger9710, logger9710_1, logger9710_2, logger9710_3, logger9710_4)

#datetime format
logger_moisture$DateTime <- as.POSIXct(paste(logger_moisture$DateTime), format = "%Y-%m-%d %H:%M:%S")
#plotting 
P <- ggplot(data = logger_moisture, aes(x = DateTime, y = water_m3_m3)) + 
  geom_point(size=1)

ggplot(data = logger_moisture, aes(x = DateTime, y = water_m3_m3)) + 
  geom_point(size=1)
#plotting moisture and water together 
library(ggplot2)

# Define scaling function for temperature

scale_temp <- function(temp) sqrt(temp)
scale_temp(logger_moisture$temp)
scale_temp <- function(temp) rank(temp)
scale_temp(logger_moisture$temp)

# Create the plot
ggplot(data = logger_moisture, aes(x = DateTime)) +
  # Plot water_m3_m3 on the left y-axis
  geom_point(aes(y = water_m3_m3), size = 1, color = "blue") +
  # Add secondary axis for temperature
  scale_y_continuous(name = "Water (m3/m3)", sec.axis = sec_axis(~ scale_temp(logger_moisture$temp), name = "Temperature (°C)")) +
  # Plot temperature on the right y-axis with scaling function
  geom_line(aes(y = scale_temp(temp)), color = "red")#I try to plot 2-y axis with temperature and moisture
#its said is " Transformation for secondary axes must be monotonic"

P <- ggplot(data = logger_moisture, aes(x = DateTime, y = water_m3_m3)) + 
  geom_point(size=1, color = "blue")
P + scale_y_continuous(sec.axis = sec_axis(~scale_temp(logger_moisture$temp))) #the same happen here 

  
#plot 2 just moisture 
ggplot(data = logger_moisture, aes(x = DateTime, y = water_m3_m3)) + 
  geom_point(size=2, color = "lightblue")+
  labs(x = "Date", y = "moisture [cm3_cm3]") +
  ggtitle("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
logger_moisture$DateTime <- round_date(logger_moisture$DateTime, "2hours")
#plot temperature 
ggplot(data = logger_moisture, aes(x = DateTime, y = water_m3_m3)) + 
  geom_point(size=2, color = "turquoise3")+
  labs(x = "Date", y = "Moisture [cm3_cm3]") +
  ggtitle("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

logger_moisture$plot <- "12"
View(logger_moisture)

#TEMPERATURE 
library(readr)
Ibutton_ <- read_csv("C:/Users/marti/Downloads/Ibutton .csv", 
                     col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                      time = col_time(format = "%H:%M:%S"), 
                                      temp = col_number()))
air_temp <- read_csv("C:/Users/marti/Downloads/air_temp.csv", 
                     col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                      time = col_time(format = "%H:%M:%S"), 
                                      temp = col_number()))
soil_temp <- read_csv("C:/Users/marti/Downloads/soil_temp.csv", 
                      col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                       time = col_time(format = "%H:%M:%S"), 
                                       temp = col_number()))
Ibutton_$name <- "plant"
air_temp$name <- "air"
soil_temp$name <- "soil"

View(Ibutton_)
colnames(soil_temp) <- c("plotn", "stage", "date", "time", "temp", "name")

DataTemp <- rbind(Ibutton_, air_temp, soil_temp)

DataTemp$DateTime <- as.POSIXct(paste0(DataTemp$date, DataTemp$time), format = "%Y-%m-%d %H:%M:%S")
View(DataTemp)#Ibutton data includes all the plots 

#plot
P <- ggplot(data = DataTemp, aes(x = DateTime, y = temp, group= plotn, color = plotn)) + 
  geom_point(size=1)+
  labs(x = "Date", y = "Temperature [ºC]") +
  ggtitle("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color = guide_legend(title = "Plot")) #Check the data because we have 85C
P + facet_wrap(~name)
P + facet_wrap(name)

boxplot(Ibutton_$temp)

write.csv(logger_moisture, "logger_moisture.csv")
write.csv(DataTemp, "DataTemp.csv")
