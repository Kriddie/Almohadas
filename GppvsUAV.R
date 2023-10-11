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
colnames(CO2_df) <- c("Date","Time","Site","Plot","Plant_Stage","Veg_vol","Air_vol",
                      "CH4_day","CH4_night","AirTemp_C","SoilTemp_C",
                      "RH","Dewpoint_C","Subsoil_percent","Subsoil_pound",
                      "Radiation_rep1_Wm2","Radiation_rep2_Wm2","Radiation_rep3_Wm2","Radiation_rep4_Wm2",
                      "comments")
