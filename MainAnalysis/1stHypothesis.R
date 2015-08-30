######################
### 1st HYPOTHESIS ###
######################
### This script tests the first hypothesis,
### that is the one concerning association between average entropy
### and social capital measures and assuming that it has quadratic form

### Load packages
library(car)
library(MASS)
library(psych)
library(QuantPsyc)
library(lattice)
library(latticeExtra)
library(reshape2)
library(lmtest)
library(lme4)
library(doBy)
library(dplyr)
library(outliers)
library(DMwR)
library(gvlma)
library(robustbase)
source("HelperFunctionsMisc/ComputingMisc.R")
source("Places/PlacesHelper.R")

# Load the data
load("Places/PlaceData.RData") # load the place data
load("Places/Places.RData") # load the place indications dataset
load("MainData/MainData11.RData") # main dataset
load("Networks/IncidenceMatrix.RData")
D.back = D # backup dataset

### Entropy measure with threshold of popularity 5 imposed on places
Ent5 <- heterogeneityCoefs(Pdat, D, P, threshold=5)
### Entropy measure with threshold of popularity 2 imposed on places
Ent2 <- heterogeneityCoefs(Pdat, D, P, threshold=2)
D$ent_avg5 <- Ent5$ent_avg # save average entropy compute with the treshold
D$ent_avg2 <- Ent2$ent_avg # save average entropy compute with the treshold


################
### ANALYSIS ###
################
D.backup <- D
D <- D[!is.na(D$ent_avg), ]
E = D[, c("id", "resmob", "soccont", "ent_avg")]
E = melt(E, id=c("id", "ent_avg"))
# Scatterplots
xyplot(value ~ ent_avg | variable, data=E,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
# Scatterplots in groups
xyplot(resmob ~ ent_avg | cluster, data=D,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(soccont ~ ent_avg | cluster, data=D,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

### Group means
summaryBy(ent_avg + ent_avg5 + ent_avg2 + soccont + resmob ~ cluster, data=D, FUN=mean, na.rm=TRUE)

### Correlation of entropy with places and social capital scales
lowerCor(D[, c("ent_avg", "ent_avg2", "ent_avg5", "places", "resmob", "soccont")]) # very low correlations


### RESMOB scale
xyplot(resmob ~ ent_avg, data=D,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(resmob ~ ent_avg | cluster, data=D,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
### Linear model
resmob.lm <- lm(resmob ~ cluster + ent_avg, data=D)
# Some diagnostics
# preliminary global test
gvlma(resmob.lm)
qqPlot(resmob.lm$residuals)
shapiro.test(resmob.lm$residuals) # not normal nosie; not good
bptest(resmob.lm) # heteroscedastic
# No autocorrelations
dwtest(resmob.lm) # no autocorrelations
# Outliers
outlierTest(resmob.lm)

### Quadratic model
resmob.qm <- lm(resmob ~ cluster + ent_avg + I(ent_avg^2), data=D)
# preliminary global test
# Some diagnostics
gvlma(resmob.qm)
qqPlot(resmob.qm$residuals)
shapiro.test(resmob.qm$residuals) # not normal nosie; not good
bptest(resmob.qm) # heteroscedastic
# No autocorrelations
dwtest(resmob.qm) # no autocorrelations
# Outliers
outlierTest(resmob.qm)

### Since there heteroscedasticity and nonnormal residuals I use robust regression
### Models with bi-square robust weights
### Linear model
resmob.rlm <- lmrob(resmob ~ cluster + ent_avg, data=D)
Anova(resmob.rlm)
qqPlot(resmob.rlm$residuals)
shapiro.test(resmob.rlm$residuals)
outlierStats(resmob.rlm)

### Quadratic model
resmob.rqm <- lmrob(resmob ~ cluster + ent_avg + I(ent_avg^2), data=D)
Anova(resmob.rqm)
qqPlot(resmob.rqm$residuals)
shapiro.test(resmob.rqm$residuals)
outlierStats(resmob.rqm)
confint(resmob.rqm)

### Wald test for nested models
anova(resmob.rlm, resmob.rqm)

### So it seems that the hypothesis holds in the case of the RESMOB scale.
### There is an association with average entropy and the quadratic model is better than the linear one
### Robust consistency corrected R^2 is 8.44%


### SOCCONT scale
xyplot(soccont ~ ent_avg, data=D,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
xyplot(soccont ~ ent_avg | cluster, data=D,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })
### Linear model
soccont.lm <- lm(soccont ~ cluster + ent_avg, data=D)
# Some diagnostics
# preliminary global test
gvlma(soccont.lm) # nice no deviations from the LM assumptions
qqPlot(soccont.lm$residuals)
shapiro.test(soccont.lm$residuals) # normal noise; good
bptest(soccont.lm) # perhaps a bit heteroscedastic
# No autocorrelations
dwtest(resmob.lm) # no autocorrelations
# Outliers
outlierTest(resmob.lm)

### Quadratic model
soccont.qm <- lm(soccont ~ cluster + ent_avg + I(ent_avg^2), data=D)
# Some diagnostics
# preliminary global test
gvlma(soccont.qm) # nice no deviations from the LM assumptions
qqPlot(soccont.qm$residuals)
shapiro.test(soccont.qm$residuals) # normal noise; good
bptest(soccont.qm) # perhaps a bit heteroscedastic
# No autocorrelations
dwtest(resmob.qm) # no autocorrelations
# Outliers
outlierTest(resmob.qm)

### Since there is slight heteroscedasticity I use robust estimation (bi-square)
### Models with bi-square robust weights
### Linear model
set.seed(111)
soccont.rlm <- lmrob(soccont ~ cluster + ent_avg, data=D)
Anova(soccont.rlm)
qqPlot(soccont.rlm$residuals)
shapiro.test(soccont.rlm$residuals)
outlierStats(soccont.rlm)

### Quadratic model
set.seed(111000)
soccont.rqm <- lmrob(soccont ~ cluster + ent_avg + I(ent_avg^2), data=D)
Anova(soccont.rqm)
qqPlot(soccont.rqm$residuals)
shapiro.test(soccont.rqm$residuals)
outlierStats(soccont.rqm)
confint(soccont.rqm)

### Wald test for nested models
anova(soccont.rlm, soccont.rqm)

### So it seems that the hypothesis holds in the case of the SOCCONT scale.
### There is an association with average entropy and the quadratic model is better than the linear one
### Robust consistency corrected R^2 is 19.09%


### Summing up: Hypothesis I is not rejected.