######################
### 3rd HYPOTHESIS ###
######################
### This script tests the third hypothesis

### Load packages
library(car)
library(MASS)
library(psych)
library(QuantPsyc)
library(lattice)
library(latticeExtra)
library(reshape2)
library(lmtest)
library(lme4)
library(doBy)
library(dplyr)
library(outliers)
library(DMwR)
library(gvlma)
library(robustbase)
source("HelperFunctionsMisc/ComputingMisc.R")
source("Places/PlacesHelper.R")

# Load data
load("MainData/MainData11.RData") # main dataset
D.back = D # backup dataset
D <- D[!is.na(D$ent_avg), ] # trim data to respondents with entropy indicators only


# Part A
active.qm <- lm(attdiscovered ~ ent_avg + I((ent_avg - mean(ent_avg))^2), data=D)
# Diagnostics
gvlma(active.qm)  # Nice! Model is rather OK
qqPlot(active.qm$residuals)
shapiro.test(active.qm$residuals)
# And the model is strong! 10% of variance is retained!
# So the A part of the hypothesis is confirmed!

# Additional analysis --- partialing out the influence of social capital
active.qmex <- lm(attdiscovered ~ resmob + soccont + ent_avg + I((ent_avg - mean(ent_avg))^2), data=D)
# Diagnostics
gvlma(active.qmex)  # Nice again!
qqPlot(active.qmex$residuals)
shapiro.test(active.qmex$residuals)

# So the part A is ultimately confirmed!!! Hurray!


# Part B
everyday.qm <- lm(attgiven ~ ent_avg + I((ent_avg - mean(ent_avg))^2), data=D)
gvlma(everyday.qm)
qqPlot(everyday.qm$residuals)
shapiro.test(everyday.qm$residuals) # not good ...

# Part C
noatt.qm <- lm(attnoatt ~ ent_avg + I((ent_avg - mean(ent_avg))^2), data=D)
gvlma(noatt.qm)
qqPlot(noatt.qm)
shapiro.test(noatt.qm$residuals)    # Great!
# And the C part is confirmed too ... although the strength of the relationship is very weak.


### Visualization