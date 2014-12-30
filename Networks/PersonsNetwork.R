# Analysis of person network
library(dplyr)
library(lattice)
library(latticeExtra)
library(psych)
library(igraph)
library(reshape2)
source("Places/PlacesHelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")
source("Networks/NetworkMethods.R")

load("Networks/IncidenceMatrix.RData") # loads AM
load("MainData/MainData12.RData") # loads D
D.back = D

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
lowerCor(D[c(38:43,70:78)])
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
pdf("Networks/PersonsGraph.pdf")
par(mar=c(2,2,2,2))
plot(Gp, layout=lay3)
dev.off()


# Transitivity
trans.g = transitivity(Gp, type="global")
trans.l = transitivity(Gp, type="barrat")

# Community structure
comm = edge.betweenness.community(Gp, weights=E(Gp)$weights, directed=FALSE)
comm.dist = table(comm$membership)
memb.fac = comm$membership
memb.fac[memb.fac > 1] = 2
D$netcomm = as.factor(memb.fac)
levels(D$netcomm) = c("Main", "None")
chisq.test(table(D$netcomm, D$cluster))
# No significant association between cluster and community

# Some more tests
t.test(places ~ netcomm, data=D) # significant (duh!)
t.test(resmob ~ netcomm, data=D) # not significant
t.test(soccont ~ netcomm, data=D) # significant
t.test(ent_avg ~ netcomm, data=D) # significant!
t.test(fullent ~ netcomm, data=D) # significant
t.test(cultcap ~ netcomm, data=D) # significant!
t.test(connections ~ netcomm, data=D) # significant (duh!)
t.test((-can1) ~ netcomm, data=D) # significant!
t.test(can2 ~ netcomm, data=D) # not significant
t.test(civic ~ netcomm, data=D) # not significant
t.test(age ~ netcomm, data=D) # significant
t.test(income ~ netcomm, data=D) # not significant
t.test(attgen ~ netcomm, data=D) # significant!
t.test(attgiven ~ netcomm, data=D) # not significant
t.test(attdiscovered ~ netcomm, data=D) # significant
t.test(attnoatt ~ netcomm, data=D) # not significant

# very awesome results

# Assortativity measures
V(Gp)$cluster = D$cluster
assortativity.degree(Gp, directed=FALSE) # no correlation
assortativity.nominal(Gp, type=D$cluster, directed=FALSE) # no correlation

# Save the dataset D
save(D, file="MainData/MainData13.RData")
save.image("Networks/PersonsGraph.RData")

rm(list=ls())

# This is it folks!