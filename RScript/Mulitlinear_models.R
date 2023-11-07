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

##What's up with + (1|Plot) ?:
#with this notation we assume an intercept that’s different for each subject” … and “1” stands for the intercept here. 
#You can think of this formula as telling your model that it should expect that there’s going to be multiple responses per subject, 
#and these responses will depend on each subject’s baseline level. 
#This effectively resolves the non-independence that stems from having multiple responses by the same subject.

#I think we should scale all variables for interpretation?

model2 <- lmer(NEEh ~ scale(radiation) +
                 scale(subsoil) +
              #       scale(airtemp) +
                 scale(soiltemp) + 
              #      scale(richness) + 
             #       scale(stage) + 
                 scale(plantago) + 
             #       scale(rh) +
                 (1|plot)
               , data = cushion )
summary(model2)
md <- modelDiagnostics(M2, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)
modelPerformance(model2)
