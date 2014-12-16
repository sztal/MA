# This script perform exploration and testing of the hypothesis that social space heterogeneity should be positively related to social capital measures.
library(car)
library(lme4)
library(psych)
library(lattice)
library(latticeExtra)
library(reshape2)
source("HelperFunctionsMisc/ComputingMisc.R")

# Load the data
load("MainData/MainData10.RData")
D.back = D # backup dataset
D = D.back[, c(1, 38:43, 68:73, 76)]
E = D[, c(1:3, 11:13)]
# normalize the data in order to present it on a common scale
for(i in 4:dim(E)[2]) E[, i] = center(E[, i], norm=TRUE)
E = melt(E, id=c("id", "resmob", "soccont"))

# Scatterplots
xyplot(resmob ~ value | variable, data=E,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

xyplot(soccont ~ value | variable, data=E,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

# Preliminary linear models
D2 = cbind(D, D[,11:13]^2)
names(D2)[14:16] = c("ent_total2", "ent_avg2", "ent_wgh2")
resmob.total = lm(resmob ~ ent_total + ent_total2, data=D2) # poor fit
resmob.avg = lm(resmob ~ ent_avg + ent_avg2, data=D2) # very poor fit
resmob.wgh = lm(resmob ~ ent_wgh + ent_wgh2, data=D2) # very poor fit :(
# The poor fit is due to outliers

soccont.total = lm(soccont ~ ent_total + ent_total2, data=D2)
soccont.avg = lm(soccont ~ ent_avg + ent_avg2, data=D2)

# Scatterplots with places as a predictor
xyplot(resmob ~ places, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

xyplot(soccont ~ places, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

mod1 = lm(soccont ~ places + ent_total * cluster, data=D)