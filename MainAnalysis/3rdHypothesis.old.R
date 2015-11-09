######################
### 3rd HYPOTHESIS ###
######################
### This script tests the third hypothesis
### that is the that claims that abstract entropy is a worse predictor than in-place average entropy

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

# Load the data
load("Places/PlaceData.RData") # load the place data
load("Places/Places.RData") # load the place indications dataset
load("MainData/MainData11.RData") # main dataset
load("Networks/IncidenceMatrix.RData")
D.back = D # backup dataset
D <- D[!is.na(D$ent_avg), ]
D$attdisccent <- D$attdiscovered - mean(D$attdiscovered)

### Attachment scales
summary(D[, grep("att", names(D), perl=TRUE)])
corr.test(D[, grep("resmob|soccont|att", names(D), perl=TRUE)])

### Only general scale and discovered scale have at least some correlation with the RESMOB and SOCCONT scales. However, attgen scale is too large extend truncated and should not be used. Thus, the discovered scale will be used.

##### RESMOB #####
##### Models #####
set.seed(171)
resmob.rqm <- lmrob(resmob ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
### Extended model
resmob.rqmex <- lmrob(resmob ~ (ent_avg + I((ent_avg-mean(ent_avg))^2))*attdisccent, data=D)
resmob.rqmatt <- lmrob(resmob ~ ent_avg + I((ent_avg-mean(ent_avg))^2) + attdisccent, data=D)
Anova(resmob.rqmex)
anova(resmob.rqmex, resmob.rqmatt)


### Attachment has no interaction with the RESMOB scale


##### SOCCONT #####
##### Models  #####
soccont.rqm <- lmrob(soccont ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
### Extended model
soccont.rqmex <- lmrob(soccont ~ (ent_avg + I((ent_avg-mean(ent_avg))^2))*attdisccent, data=D)
soccont.rqmatt <- lmrob(soccont ~ ent_avg + I((ent_avg-mean(ent_avg))^2) + attdisccent, data=D)
Anova(soccont.rqmex)
anova(soccont.rqmatt, soccont.rqmex)

### Attachment has no interaction with the RESMOB scale