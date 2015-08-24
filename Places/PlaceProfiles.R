# This scrip generates a dataset with place profiles

source("Places/PlacesHelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")
library(dplyr)

load("MainData/MainData9.RData") # loads the mains dataset D
load("Networks/IncidenceMatrix.RData") # load the people-places indicence matrix AM
load("Places/Places.RData")

# Intialize a place dataset (places as rows) with one variable - place popularity which is the number of respondent indicating a place
Pdat = data.frame(popularity = apply(AM[, ], 2, sum))
# Get full profiles, dominant cluster and entropy of places
# This may take a while - so please be patient
Pdat = getFullPlaceInfo(Pdat, D, P)

# Save the dataset
write.csv(Pdat, file="Places/PlaceData.csv", row.names=TRUE)
save(Pdat, file="Places/PlaceData.RData")

rm(list=ls())
# This is it folks!