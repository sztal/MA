# Analysis of person network
library(dplyr)
library(lattice)
library(latticeExtra)
library(psych)
library(igraph)
library(reshape2)
source("Places/PlacesHelper.R")
source("GenCompFuncs/ComputingMisc.R")
source("Networks/NetworkMethods.R")

load("Networks/IncidenceMatrix.RData") # loads AM
load("MainDat/mainDat2.RData") # loads D

IM = AM # rename it to the proper name - incidene matrix
# Compute person adjacency matrix
AM = IM %*% t(IM)
places = diag(AM) # number of places indicated by persons

# Derive a graph object
Gp = graph.adjacency(AM, mode="undirected", weighted=TRUE, diag=FALSE, add.rownames=TRUE)

# Some statistics
dd = degree(Gp) # degree distribution
# number of conncetions to the D dataset
D$connections = dd
# Some correlations
lowerCor(D[c(38:43,70:74)]) # nothing interesting since this a function of # of places
histogram(dd, xlab="# of connections") # very normal!
# but of course not exactly
shapiro.test(dd) # normality rejected

# Now some cosmetics for vertices - persons
V(Gp)$label.color = rgb(0,0,.2,.5)
V(Gp)$label.cex = .3
V(Gp)$connections = dd
V(Gp)$size = 1+sqrt(dd)
V(Gp)$frame.color = rgb(.1,.1,.8,.5)
V(Gp)$color = rgb(.8,.2,0,.5)
V(Gp)$label = NA
# Some cosmetics for edges
E(Gp)$color = rgb(.3,.3,0,.1)
# Plotting
lay <- layout.fruchterman.reingold(Gp, coolexp=1, niter=1000, area=vcount(Gp)^4)
lay3 <- layout.kamada.kawai(Gp, coolexp=1.5, niter=1000, area=vcount(Gp)^3.5)
pdf("Pictures/PersonsGraph.pdf")
par(mar=c(2,2,2,2))
plot(Gp, layout=lay3)
dev.off()
