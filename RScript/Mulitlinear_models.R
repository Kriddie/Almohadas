##this is a script that can be used to run multilinear, mixed models
#also contains code to run diagnostics

#excellant tutorial, includes dragons
# https://ourcodingclub.github.io/tutorials/mixed-models/

##mulitvariate model building
# https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html


library(ggplot2)
library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)


cushion <- read.csv(here::here("FieldData/CO2_update_2023-11-05.csv"))

###multilinear nodel

#I think we should scale all variables for interpretation?

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