#this r script is to explore the relationship between radiation and GPP
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

CO2_df <- read.csv(here::here("FieldData/CO2_FieldData.csv"))
colnames(CO2_df) <- c("Date","Time","Site","Plot","Plant_Stage","Veg_vol","Air_vol",
                      "NEE_mol.m2.s","ER_mol.m2.s","NEE_gCO2.m2.h1","ER_gCO2.m2.h1",
                      "GPP_gCO2.m2.h1","CO2_day_RAW","CO2_night_RAW","AirTemp_C","SoilTemp_C",
                      "RH","Dewpoint_C","Subsoil","Subsoil_WaterContent",
                      "Radiation_rep1_Wm2","Radiation_rep2_Wm2","Radiation_rep3_Wm2","Radiation_rep4_Wm2",
                      "comments")

CH4_df <- read.csv(here::here("FieldData/CH4_FieldData.csv"))
colnames(CH4_df) <- c("Date","Time","Site","Plot","Plant_Stage","Veg_vol","Air_vol",
                      "CH4_day","CH4_night","AirTemp_C","SoilTemp_C",
                      "RH","Dewpoint_C","Subsoil_percent","Subsoil_pound",
                      "Radiation_rep1_Wm2","Radiation_rep2_Wm2","Radiation_rep3_Wm2","Radiation_rep4_Wm2",
                      "comments")

#fix date
CO2_df$Date <- as.Date(CO2_df$Date, format = "%m/%d/%y")
CH4_df$Date <- as.Date(CH4_df$Date, format = "%m/%d/%y")

#fix data format
CO2_df$Radiation_rep1_Wm2 <- as.numeric(CO2_df$Radiation_rep1_Wm2)
CO2_df$Radiation_rep2_Wm2 <- as.numeric(CO2_df$Radiation_rep2_Wm2)
CO2_df$Radiation_rep3_Wm2 <- as.numeric(CO2_df$Radiation_rep3_Wm2)
CO2_df$Radiation_rep4_Wm2 <- as.numeric(CO2_df$Radiation_rep4_Wm2)

CO2_df$Plot <- tolower(CO2_df$Plot)

CO2_df$Radiation_ave_Wm2 <- colMeans(CO2_df[ , c("Radiation_rep1_Wm2","Radiation_rep2_Wm2","Radiation_rep3_Wm2","Radiation_rep4_Wm2")])
CO2_df$Radiation_ave_Wm2 <- colMeans(CO2_df[ , c(21:24)], na.rm=TRUE)



#plot

#show these

#gpp
ggplot(CO2_df ,aes(x=Radiation_rep2_Wm2,y=abs(GPP_gCO2.m2.h1))) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CO2_df ,aes(x=log1p(Radiation_rep2_Wm2),y=log1p(abs(GPP_gCO2.m2.h1)))) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CO2_df ,aes(x=log1p(Radiation_rep2_Wm2),y=log1p(SoilTemp_C))) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CO2_df ,aes(x=log1p(Radiation_rep2_Wm2),y=log1p(AirTemp_C))) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 

#ER
ggplot(CO2_df ,aes(x=log1p(SoilTemp_C),y=log1p(ER_mol.m2.s))) +
  geom_point(aes(color= as.character(Plot)),size=4)+                                     
  theme_bw(base_size = 16) 





#scratch

#CH4
ggplot(CH4_df%>%filter(CH4_night>0) ,aes(x=SoilTemp_C,y=CH4_night)) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CH4_df%>%filter(CH4_day< 3.35e-06) ,aes(x=SoilTemp_C,y=CH4_day)) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 

#CO2
ggplot(CO2_df ,aes(x=SoilTemp_C,y=GPP_gCO2.m2.h1)) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CO2_df ,aes(x=AirTemp_C,y=ER_mol.m2.s)) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CO2_df ,aes(x=Radiation_rep2_Wm2,y=AirTemp_C)) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
ggplot(CO2_df ,aes(x=Radiation_rep2_Wm2,y=SoilTemp_C)) +
  geom_point(aes(color=as.character(Date)),size=4)+                                     
  theme_bw(base_size = 16) 



ggplot(CO2_df ,aes(x=log1p(AirTemp_C),y=log1p(ER_mol.m2.s))) +
  geom_point(aes(color=Plot),size=4)+                                     
  theme_bw(base_size = 16) 
