# This script perform exploration and testing of the hypothesis that social space heterogeneity should be positively related to social capital measures.
library(car)
library(lme4)
library(psych)
library(lattice)
library(latticeExtra)
library(reshape2)
source("HelperFunctionsMisc/ComputingMisc.R")

# Load the data
load("MainData/MainData11.RData")
D.back = D # backup dataset
D = D.back[, c(1, 38:43, 68:72, 76)]
E = D[, c(1:3, 11:12)]
# normalize the data in order to present it on a common scale
for(i in 2:dim(E)[2]) E[, i] = center(E[, i], norm=TRUE)
E = melt(E, id=c("id", "resmob", "soccont"))

# Scatterplots
xyplot(resmob ~ value | variable, data=E,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       }) # outliers seem to have strong influence

xyplot(soccont ~ value | variable, data=E,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       }) # linear in total and quadratic in average entropy?

