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

# Get the first variable for a dataset in which places are observation units (rows)
Pdat = data.frame(popularity = apply(AM[, -1], 2, sum))
# Get full profiles, dominant cluster and entropy of places
Pdat = getFullPlaceInfo(Pdat, D, P)

# Compute social space heterogeneity indices (various entropy measures) for persons as functions of entropy and entropy distributions of places indicated by them
E = heterogeneityCoefs(Pdat, D, P)
D = cbind(D, E)
# Check the distributions
histogram(~ent_total, data=D, xlab="") # nice quite symmetric distribution
histogram(~ ent_avg, data=D, xlab="") # nice quite symmetric distrubtion
histogram(~ ent_max + ent_min, data=D, xlab="") # skewed weird distributions
# summary of entropy measures distributions
summary(D[, 70:73])

# Preliminary correlations with place attachment and social capital and civic activity
lowerCor(D[, c(70:73, 37:43)]) # there are some potentially interesting associations
# Some preliminary bivariate graphs
# Social contacts vs. total and average entropy
xyplot(soccont ~ ent_total, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(soccont ~ sqrt(ent_total), data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(soccont ~ ent_avg, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(soccont ~ sqrt(ent_avg), data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       }) # bad fit
# Resource mobilization vs. total and average entropy
xyplot(resmob ~ ent_total, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(resmob ~ sqrt(ent_total), data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(resmob ~ ent_avg, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(resmob ~ sqrt(ent_avg), data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       }) # bad fit
# Histograms of social capital
histogram(~ resmob + soccont, data=D, xlab="")
# resmob has limited variability
# Clustering of social capital scales and entropy measures?
# Social capital
d1 = D[, c("resmob", "soccont", "cluster")]
d1 = melt(d1, id.vars="cluster")
bwplot(value ~ cluster | variable, data=d1, xlab="") # there are obvious clusterings!
# Entropy measures (total and average)
bwplot(ent_total ~ cluster, data=D, xlab="") # some clusterings
bwplot(ent_avg ~ cluster, data=D, xlab="")

# Preliminary check of association between social capital, entropy and place attachment
soccont.mod1 <- lm(soccont ~ center(sqrt(ent_total)) + center(attgen), data=D)
soccont.mod2 <- lm(soccont ~ center(sqrt(ent_total)) * center(attgen), data=D)
anova(soccont.mod1, soccont.mod2) # interaction is not significant
# Preliminary check of association between social capital, entropy and place attachment
resmob.mod1 <- lm(resmob ~ center(sqrt(ent_total)) + center(attgen), data=D)
resmob.mod2 <- lm(resmob ~ center(sqrt(ent_total)) * center(attgen), data=D)
anova(resmob.mod1, resmob.mod2) # interaction is not significant

# The same but for average entropy
# Preliminary check of association between social capital, entropy and place attachment
soccont.mod1 <- lm(soccont ~ center(ent_avg) + center(attgen), data=D)
soccont.mod2 <- lm(soccont ~ center(ent_avg) * center(attgen), data=D)
anova(soccont.mod1, soccont.mod2) # interaction is not significant
# Preliminary check of association between social capital, entropy and place attachment
resmob.mod1 <- lm(resmob ~ center(sqrt(ent_avg)) + center(attgen), data=D)
resmob.mod2 <- lm(resmob ~ center(sqrt(ent_avg)) * center(attgen), data=D)
anova(resmob.mod1, resmob.mod2) # not significant interaction

summary(Pdat$popularity)
# Check the distribution
histogram(~ popularity, data=Pdat, xlab="") # very skewed; a power-law?

# Save the new dataset
save(Pdat, file="MainDat/mainPdat1.RData")
write.csv(Pdat, file="MainDat/mainPdat1.csv", row.names=TRUE)
# Save the updated dataset
save(D, file="MainDat/mainDat2.RData")
write.csv(D, file="MainDat/mainDat2.csv", row.names=FALSE)
# Save the people-place adjacency matrix AM
save(AM, file="Networks/People_Place_AM.RData")
write.csv(AM, file="Networks/People_Place_AM.csv", row.names=FALSE)

####################################################################
###   Power-law distribution test for popularity of the places   ###
####################################################################

# Load the data (just in case)
load("MainDat/mainDat2.RData")
load("MainDat/mainPdat1.RData")
load("Networks/People_Place_AM.RData")
P <- read.csv("MainDat/PlacesFinal.csv")
P = toCharacter(P)
# Frequency distribution of popularity
pop_dist = prop.table(table(Pdat$popularity))
# logged probabilities and popularities (base 10)
log_pop_pr = as.numeric(log10(pop_dist)) # logged probabilities
log_pop = log10(as.numeric(names(pop_dist))) # logged popularities
# scatterplot
xyplot(log_pop_pr ~ log_pop,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y~ x))
             }) # seems like power-law-like distribution!

# Maximum likelihood estimation of the linear coefficient
# OLS estimation is not recommended (Fronczak and Fronczak, 2009)
x = Pdat$popularity
xpr = mapPr(x)
n = length(x)
# MLE estimate
# Fit the power-law
fit = power.law.fit(Pdat$popularity, xmin=1, implementation="plfit")
fit$KS.p  # Kolmogorov-Smirnov test proves that the data has power-law distribution
fit2 = power.law.fit(Pdat$popularity, xmin=1, implementation="R.mle")
alpha.mle = fit$alpha
# Now compute the constant (log of it is the constant in the mle regresion model)
C = (1 - alpha) / (-1) # this can be derived from the fact that propability density function integral has to equal to 1

# OLS estimator
fit.ols = lm(log10(xpr) ~ log10(x)) # almost 99% of variance retained
alpha.ols = -coef(fit.ols)[[2]]
# Scatterplot
xyplot(xpr ~ x, xlab="Popularność (X)", ylab="P(X)",
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

# So summing up: there is a power-law to the distribution of the popularity of places!


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
pdf("Pictures/bipartite.pdf")
par(mar=c(2,2,2,2))
plot(Gpp, layout=lay)
dev.off()

# Save the adjacency (incidene) network
save(AM, file="Networks/IncidenceMatrix.RData")

# This is it folks!
rm(list=ls())