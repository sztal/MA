# Real entropy
# This script computes the real entropy measure. It is defined as entropy of the distribution of all (unique) persons (in terms of their cluster assignments) that are met through place that are being visited by the respondent
source("HelperFunctionsMisc/ComputingMisc.R")
source("Places/PlacesHelper.R")
library(dplyr)
library(lattice)

load("MainData/MainData11.RData") # load the main dataset D
load("Places/PlaceData.RData") # load data on places
load("Places/Places.RData") # load the place indications datastet
load("Networks/IncidenceMatrix.RData") # load the incidence matrix

# Compute real entropy measures
realent = realEntropy(D, AM)