# This script derives bipartite network (peoples-place) from the places dataset and the respondents dataset ("Places/Places.RData") and ("MainData9.RData")

library(dplyr)
library(lattice)
library(latticeExtra)
library(psych)
library(igraph)
library(reshape2)
source("Places/PlacesHelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")
source("Networks/NetworkMethods.R")

load("MainData/MainData9.RData") # load the main respondents dataset as an object named D
load("Places/Places.RData") # load the final place dataset
P = arrange(P, id)
D = arrange(D, id)

231 == sum(P$id == D$id) # ids are correct

P = toCharacter(P) # change all place columns to character
Places = uniquePlaces(P) # this a list which have the full set of palces as the first element and the list of unique place names as the second element

AM = getPeoplePlaceMatrix(P) # get people-place incidnce matrix (double indications are counted as one); rownames are respondents ids
sum(AM) # 1680 unique place indications

# Save the biparite people-place incidence matrix
write.csv(AM, file="Networks/IncidenceMarix.csv", row.names=TRUE)
save(AM, file="Networks/IncidenceMatrix.RData")


############################################
###   Bipartite Network: people-places   ###
############################################

# I use adjacency matrix AM (generated earlier); technically it is two-mode incidence matrix
# This part is purely visual; just to get accquainted with the data
# Actual analyses will be performed on individual-individual and place-place networks
Gpp = graph.incidence(AM) # graph representation of AM
persons = 1:231
places = 232:(231+dim(AM)[2])
popular = apply(AM, 2, sum)
popular = places[popular >= 5]
# Now some cosmetics for vertices (respondents)
V(Gpp)$label.color = rgb(0,0,.2,.5)
V(Gpp)$label.cex = .3
V(Gpp)$size = 3
V(Gpp)$frame.color = rgb(0,0,0,.5)
V(Gpp)$color = rgb(0,0,0,.5)
V(Gpp)$label = NA             # no labels for the respondents
# cosmetics for places
V(Gpp)$label.color[places] = rgb(.1, .1, .1, .5)
V(Gpp)$label.cex[places] = .3
V(Gpp)$size[places] = 2+sqrt(apply(AM, 2, sum))
s = V(Gpp)$size[places]
V(Gpp)$frame.color[places] = rgb(.7,.5,.2, exp(.05*s) / (1+exp(.05*s)))
V(Gpp)$color[places] = rgb(.2,.7,0,.5)
# Popular places - display names
V(Gpp)$label[popular] = colnames(AM)[popular-231]
# Some cosmetics for edges
E(Gpp)$color = rgb(.5,.5,0,.4)
# Plotting
lay <- layout.fruchterman.reingold(Gpp, coolexp=1, niter=1000, area=vcount(Gpp)^3)
pdf("Networks/bigraph.pdf")
par(mar=c(2,2,2,2))
plot(Gpp, layout=lay)
dev.off()

# Degree distribution
places.deg = apply(AM, 2, sum)
persons.deg = apply(AM, 1, sum)
histogram(places.deg, xlab="")
histogram(persons.deg, xlab="")

# This is it folks!
rm(list=ls())