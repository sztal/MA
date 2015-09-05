######################
### 2nd HYPOTHESIS ###
######################
### This script tests the second hypothesis
### that is the one about interaction effect between place attachment and entropy in the extended model from the first hypothesis.

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

# Compute entropy of thr abstract space
D$abs_ent = realEntropy(D, AM)
D.backup <- D

D <- D[!is.na(D$ent_avg), ]
D <- D[!is.na(D$abs_ent), ]

### Since I used robust MM bi-square method regression to verify the first hypothesis,
### then I have to use it here as well,
### because otherwise model would not be comparable.

##### RESMOB #####
##### Models #####
set.seed(171)
resmob.rqm <- lmrob(resmob ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)

### abstract entropy model
set.seed(717)
resmob.abs.rqm <- lmrob(resmob ~ abs_ent + I((abs_ent-mean(abs_ent))^2),
                        data=D)

### Cox test for two non-nested models
coxtest(resmob.rqm, resmob.abs.rqm)
### So in the case of the RESMOB scale the hypothesis is not rejected

##### SOCCONT #####
##### Models  #####
set.seed(111000)
soccont.rqm <- lmrob(soccont ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)

### abstract entropy model
soccont.abs.rqm <- lmrob(soccont ~ abs_ent + I((abs_ent-mean(abs_ent))^2), data=D)

### Cox test for two non-nested models
coxtest(soccont.rqm, soccont.abs.rqm)
### So in the case of the SOCCONT scale the hypothesis is not rejected

### So the hypothesis II stays!