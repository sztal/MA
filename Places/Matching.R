# This script tries to match the origianl place names to the unified list of names
source("Places/PlacesHelper.R")

load("Places/PlaceNamesWork.RData")

# Approximate matching - column by column with by hand supervision and checking

p1 = P[,1] # first column
P.back = P # Full data backup
N.back = N # backup
# For convenience N is now N.uniq
N = N.uniq

df1 = approxMatch(p1, N.uniq)
# Corrections are done in the console for convenienve - no documentation for this - sorry

# DONE !!!
write.csv(P, file="Places/PlacesFinal.csv", row.names=FALSE)

save(P, file="Places/organizePlaces.RData")
save.image(file="Places/organizePlacesWS.RData")