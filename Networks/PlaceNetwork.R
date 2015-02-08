# This script performs analysis of place network
library(dplyr)
library(lattice)
library(latticeExtra)
library(psych)
library(igraph)
library(reshape2)
library(doBy)
source("Places/PlacesHelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")
source("Networks/NetworkMethods.R")

load("Networks/IncidenceMatrix.RData") # loads AM
load("Places/PlaceData.RData") # loads the place data

AM = t(AM) %*% AM
popularity = diag(AM) # this we know already

# Derive a graph object
Gp = graph.adjacency(AM, mode="undirected", weighted=TRUE, diag=FALSE, add.rownames=TRUE)

# Some statistics
dd = degree(Gp) # maybe it is scale-free?
Pdat$connections = dd
histogram(Pdat$connections)
# Power-law test
n = dim(Pdat)[1]
fit = power.law.fit(Pdat$connections, xmin=1, implementation="plfit")
alpha.mle = fit$alpha
alpha.mle.se = (alpha.mle - 1) / sqrt(n)
C = alpha.mle - 1
# Graph with xmin = 1
x = Pdat$connections
xpr = mapPr(x)
n = length(x)
xyplot(xpr ~ x, xlab="Connections (X)", ylab="P(X)",
       scales=list(x=list(relation="free", log=10, cex=1),
                   y=list(relation="free", log=10, cex=1)),
       prepanel = function(x, y, subscripts) {
             list(xlim=c(0,2))
             list(ylim=c(min(y), max(y)))
       },
       panel = function(x, y, subscripts, ...) {
             panel.xyplot(x, y, cex=1, ...)
             panel.abline(a=log(C), b=-alpha.mle, lty=2, lwd=2)
             panel.grid(h = -1, v = -1)
             panel.rug(x = x[is.na(y)],
                       y = y[is.na(x)])
       },
       subscripts = TRUE,
       xscale.components = xscale.components.logpower,
       yscale.components = yscale.components.logpower)
# MLE with fitted xmin
fit = power.law.fit(Pdat$connections, xmin=NULL, implementation="plfit")
alpha.mle = fit$alpha
alpha.mle.se = (alpha.mle - 1) / sqrt(n)
C = alpha.mle - 1
# Graph with fitted xmin
x = Pdat$connections
xpr = mapPr(x)
n = length(x)
xyplot(xpr ~ x, xlab="Connections (X)", ylab="P(X)",
       scales=list(x=list(relation="free", log=10, cex=1),
                   y=list(relation="free", log=10, cex=1)),
       prepanel = function(x, y, subscripts) {
             list(xlim=c(0,2))
             list(ylim=c(min(y), max(y)))
       },
       panel = function(x, y, subscripts, ...) {
             panel.xyplot(x, y, cex=1, ...)
             panel.abline(a=log(C), b=-alpha.mle, lty=2, lwd=2)
             panel.grid(h = -1, v = -1)
             panel.rug(x = x[is.na(y)],
                       y = y[is.na(x)])
       },
       subscripts = TRUE,
       xscale.components = xscale.components.logpower,
       yscale.components = yscale.components.logpower)


# Now some cosmetics for vertices - persons
V(Gp)$label.color = rgb(0,0,.2,.5)
V(Gp)$label.cex = .3
V(Gp)$connections = dd
V(Gp)$size = 1+sqrt(dd)
V(Gp)$frame.color = rgb(.5,0,0,.5)
V(Gp)$color = rgb(.6,1,.6,.5)
V(Gp)$label = NA
V(Gp)$label.color = rgb(0,0,0,.75)
# Some cosmetics for edges
E(Gp)$color = rgb(.75,.75,.75,.5)
# Plotting
lay <- layout.fruchterman.reingold(Gp, coolexp=1, niter=1000, area=vcount(Gp)^4)
lay3 <- layout.kamada.kawai(Gp, coolexp=1.5, niter=1000, area=vcount(Gp)^3.5)
pdf("Networks/PlaceGraph.pdf")
par(mar=c(2,2,2,2))
plot(Gp, layout=lay)
dev.off()

# Check the most conncected places
quantile(dd, seq(0,1,.05))

hubs = names(dd[dd >= 73.7])
# Graph with names of hubs
V(Gp)[hubs]$label = hubs
V(Gp)$label.cex = .6
pdf("Networks/PlaceGraph.pdf")
par(mar=c(2,2,2,2))
plot(Gp, layout=lay3)
dev.off()

# Transitivity
trans.g = transitivity(Gp, type="global")
trans.l = transitivity(Gp, type="local")
between = betweenness(Gp) # powerlaw?
histogram(between)
Pdat$betweenness = between
# In general it is weird...

# Power law check for betweenness
fit = power.law.fit(Pdat$betweenness, xmin=NULL, implementation="plfit")
alpha.mle = fit$alpha
alpha.mle.se = (alpha.mle - 1) / sqrt(n)
C = alpha.mle - 1
# Graph with fitted xmin
x = Pdat$betweenness
xpr = mapPr(x)
n = length(x)
xyplot(xpr ~ x, xlab="Connections (X)", ylab="P(X)",
       scales=list(x=list(relation="free", log=10, cex=1),
                   y=list(relation="free", log=10, cex=1)),
       prepanel = function(x, y, subscripts) {
             list(xlim=c(0,2))
             list(ylim=c(min(y), max(y)))
       },
       panel = function(x, y, subscripts, ...) {
             panel.xyplot(x, y, cex=1, ...)
             panel.abline(a=log(C), b=-alpha.mle, lty=2, lwd=2)
             panel.grid(h = -1, v = -1)
             panel.rug(x = x[is.na(y)],
                       y = y[is.na(x)])
       },
       subscripts = TRUE,
       xscale.components = xscale.components.logpower,
       yscale.components = yscale.components.logpower)

# Community detection
comm = edge.betweenness.community(Gp, weights=E(Gp)$weights, directed=FALSE)
Pdat$netcomm = comm$membership

# (Dys)assortativity
d = assortativity.degree(Gp) # slightly negative
# Permutation test for the correlation
q = permtest(AM, n=1000, FUN=assortativity.degree, directed=FALSE)
quantile(q, c(0.05, 0.95))
shapiro.test(q)
mean(q)
sd(q)

# Save the dataset
save(Pdat, file="Places/PlaceData.RData")

# Save the workspace
save.image("Networks/PlaceNetwork.RData")

rm(list=ls())

# This is it folks!