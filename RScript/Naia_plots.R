##NUEVOS DATOS ACTUALIZADOS###
library(here)
library(readxl)
library(ggplot2)

cushion <- read.csv(here::here("FieldData/CO2_update_2023-11-05.csv"))

#Run ANOVA model
model <- aov(NEEh ~ stage, data = cushion)

#ANOVA assumptions: If these assumptions aren’t met, then the results of our one-way ANOVA could be unreliable.
#1. Normality – Each sample was drawn from a normally distributed population.
#2. Equal Variances – The variances of the populations that the samples come from are equal.
#3. Independence – The observations in each group are independent of each other and the observations within groups were obtained by a random sample.

#####
#ASSUMPTION #1: check for Normality
#ANOVA assumes that each sample was drawn from a normally distributed population.
######
hist(cushion$NEEh)

#create Q-Q plot to compare this dataset to a theoretical normal distribution 
#In general, if the data points fall along a straight diagonal line in a Q-Q plot, then the dataset likely follows a normal distribution
qqnorm(model$residuals)
#add straight diagonal line to plot
qqline(model$residuals)

#Conduct Shapiro-Wilk Test for Normality.
#The Shapiro-Wilk Test tests the null hypothesis that the samples come from a normal distribution vs. the alternative hypothesis that the samples do not come from a normal distribution. 
#<.05 p-value means it comes from a non-normal distribution
#if you have extremely large sample sizes then statistical tests like the Shapiro-Wilk test will almost always tell you that your data is non-normal.
shapiro.test(cushion$NEEh)

#notes:
#In general, a one-way ANOVA is considered to be fairly robust against violations of the normality assumption as long as the sample sizes are sufficiently large. 
#it’s often best to inspect your data visually using graphs like histograms and Q-Q plots. 
#By simply looking at the graphs, you can get a pretty good idea of whether or not the data is normally distributed.

##NOT NORMAL??##
#If the normality assumption is severely violated or if you just want to be extra conservative, you have two choices:
#(1) Transform the response values of your data so that the distributions are more normally distributed.
#(2) Perform an equivalent non-parametric test such as a Kruskal-Wallis Test that doesn’t require the assumption of normality.

###############
#Assumption #2: Equal Variance
#ANOVA assumes that the variances of the populations that the samples come from are equal.
###############
#Check the assumption visually using boxplots.
boxplot(NEEh ~ stage, data=cushion)
#Check the assumption using a formal statistical tests like Bartlett’s Test
#Bartlett’s Test tests the null hypothesis that the samples have equal variances vs. the alternative hypothesis that the samples do not have equal variances. 
bartlett.test(NEEh ~ stage, data=cushion)

##NOT EQUAL VARIANCE?##
#a one-way ANOVA is considered to be fairly robust against violations of the equal variances assumption as long as each group has the same sample size.
#However, if the sample sizes are not the same and this assumption is severely violated, you could instead run a Kruskal-Wallis Test, which is the non-parametric version of the one-way ANOVA.

###############
#Assumption #3: Independence
###############
#ANOVA assumes The observations in each group are independent of the observations in every other group.
#The observations within each group were obtained by a random sample.

#How to check this assumption:
#There is no formal test you can use to verify that the observations in each group are independent and that they were obtained by a random sample. 
#The only way this assumption can be satisfied is if a randomized design was used.

##NOT INDEPENDENT??##
#What to do if this assumption is violated:
#Unfortunately, there is very little you can do if this assumption is violated. 
#Simply put, if the data was collected in a way where the observations in each group are not independent of observations in other groups, or if the observations within each group were not obtained through a randomized process, the results of the ANOVA will be unreliable.



##plotting stage vs reponse variables
stageNEEh<-ggplot(cushion, aes(stage, NEEh, group=stage, color=NEEh))+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun.data=mean_se, geom="errorbar")+
  theme_bw()+
  guides(fill=guide_legend(title="stage"))
stageNEEh

plot_alt <- ggplot(cushion, aes(stage, NEEh, group=stage, fill=stage))+
  geom_boxplot()
plot_alt

model1 <- aov(NEEh ~ stage, data = cushion)
summary(model1)

stageGPPh<-ggplot(cushion, aes(stage, GPPh, group=stage, color=GPPh))+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun.data=mean_se, geom="errorbar")+
  theme_bw()+
  guides(fill=guide_legend(title="stage"))
stageGPPh

plot_alt2 <- ggplot(cushion, aes(stage, GPPh, group=stage, color=GPPh))+
  geom_boxplot()
plot_alt2

model2 <- aov(GPPh ~ stage, data = cushion)
summary(model2)

stageERh<-ggplot(cushion, aes(stage, ERh, group=stage, color=ERh))+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun.data=mean_se, geom="errorbar")+
  theme_bw()+
  guides(fill=guide_legend(title="stage"))
stageERh

plot_alt3 <- ggplot(cushion, aes(stage, ERh, group=stage, color=GPPh))+
  geom_boxplot()
plot_alt3


stageplantago<-ggplot(cushion, aes(stage, plantago, group=stage, color=plantago))+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun.data=mean_se, geom="errorbar")+
  theme_bw()+
  guides(fill=guide_legend(title="stage"))
stageplantago

plot_alt4 <- ggplot(cushion, aes(stage, plantago, group=stage, color=GPPh))+
  geom_boxplot()
plot_alt4

###multilinear regression
 

ggplt <- ggplot(cushion, aes(x=radiation, y=NEEh#, shape=stage
                             ))+ 
  geom_point()+ 
  theme_classic() 

ggplt 

# Plotting multiple Regression Lines 
ggplt+geom_smooth(method=lm,se=TRUE,fullrange=TRUE, 
                  aes(color=stage)) 


##plantago y NEEh with normal regrression
model1 <- ggplot(cushion, aes(richness, ERh)) +
  geom_point() +
  geom_smooth(method = "lm")
model1



###relation between airtemp and radiation with other variables##
##checkinng normality???
hist1<-ggplot(cushion, aes(airtemp))+
  geom_histogram(bins=10, aes(y=..density..))+
  geom_density(alpha=0.2, fill="red")

model1 <- ggplot(cushion, aes(airtemp, ERh)) +
  geom_point() +
  geom_smooth(method = "lm")

hist1<-ggplot(cushion, aes(radiation))+
  geom_histogram(bins=10, aes(y=..density..))+
  geom_density(alpha=0.2, fill="red")

model1 <- ggplot(cushion, aes(radiation, ERh)) +
  geom_point() +
  geom_smooth(method = "lm")


