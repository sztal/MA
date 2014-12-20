# This script performs analysis of the relationship between popularity of places and their heterogeneity (entropy)
library(psych)
library(lmtest)
library(car)
library(MASS)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(reshape2)
source("HelperFunctionsMisc/ComputingMisc.R")

# load the dataset
load("Places/PlaceData.RData")

# The quantiles of popularity
quantile(Pdat$popularity)

# Limit the dataset only to the highest quartile (since only then places have at least three indications, so at least a bit reliable measures of heterogeneity may be computed)
Pdat.back = Pdat
Pdat = Pdat[Pdat$popularity >= 3, ]

# Distributions of absolute and relative entropy
histogram(~ ent + r_ent, data=Pdat, xlab="")

# Correlations with popularity
lowerCor(Pdat[, c(3:4, 1)])

# Correlogram with popularity
Pdat = Pdat[, c("popularity", "ent", "r_ent")]
E = melt(Pdat, id="popularity")
xyplot(log(popularity) ~ value | variable, data=E, ylab="Popularity", xlab="Entropy",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

xyplot(log(popularity) ~ ent, data=Pdat, ylab="Popularity", xlab="Entropy",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

xyplot(log(popularity) ~ (ent), data=Pdat, ylab="Popularity", xlab="Entropy",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

xyplot(log10(popularity) ~ ent, data=Pdat, ylab="Popularity", xlab="Entropy",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.curve(.6569 + .11493*x + .07898*x^2)
             panel.loess(x, y, col="red")
       })

xyplot(popularity ~ ent, data=Pdat, ylab="Popularity", xlab="Entropy",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.loess(x, y, col="red")
             panel.abline(v=1.145091, lwd=.8, lty=2)
             panel.abline(v=.8, lwd=.8, lty=2)
       })

# Model
lm1 = lm(popularity ~ ent, data=Pdat)
lm2 = lm(I(log(popularity)) ~ ent + I(center(ent^2)), data=Pdat)
lm3 = lm(I(log(log(popularity))) ~ ent, data=Pdat)