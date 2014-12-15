# This script generates MainData10.RData dataset from MainData9.RData and Places.RData. Social space heterogeneity measures based on entropy of distributions of place visitors are added.

source("Places/PlacesHelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")
library(dplyr)
library(lattice)

load("MainData/MainData9.RData") # loads the mains dataset D
load("Networks/IncidenceMatrix.RData") # load the people-places indicence matrix AM
load("Places/PlaceData.RData") # load the place data
load("Places/Places.RData") # load the place indications dataset
 
E = heterogeneityCoefs(Pdat, D, P)
D = cbind(D, E)

# Check the distributions
histogram(~ ent_total, data=D, xlab="Total Entropy") # nice quite symmetric distribution
histogram(~ ent_avg, data=D, xlab="Average Entropy") # nice quite symmetric distrubtion
histogram(~ ent_wgh, data=D, xlab="Weighted Entropy") # a bit weird; all three measures have quite different distributions
histogram(~ ent_max + ent_min, data=D, xlab="") # skewed weird distributions

# Save the dataset
save(D, file="MainData/MainData10.RData")

rm(list=ls())

# This is it folks!