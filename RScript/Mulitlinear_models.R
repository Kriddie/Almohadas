##this is a script that can be used to run multilinear, mixed models
#also contains code to run diagnostics

library(ggplot2)
library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)


cushion <- read.csv(here::here("FieldData/CO2_update_2023-11-05.csv"))

###multilinear with statistics?
library(tidyverse)
model2 <- lmer(NEEh ~ scale(radiation) +
                 scale(subsoil) +
                 #    scale(airtemp) +
                 scale(soiltemp) + 
                 #=   scale(richness) + 
                 #   scale(stage) + 
                 scale(plantago) + 
                 #   scale(rh) +
                 (1|plot)
               , data = cushion )
summary(model2)