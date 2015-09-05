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
resmob.lm <- lm(resmob ~ ent_avg, data=D)
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
resmob.qm <- lm(resmob ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
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
set.seed(17)
resmob.rlm <- lmrob(resmob ~ ent_avg, data=D)
Anova(resmob.rlm)
qqPlot(resmob.rlm$residuals)
shapiro.test(resmob.rlm$residuals)
outlierStats(resmob.rlm)

### Quadratic model
set.seed(171)
resmob.rqm <- lmrob(resmob ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
Anova(resmob.rqm)
qqPlot(resmob.rqm$residuals)
shapiro.test(resmob.rqm$residuals)
outlierStats(resmob.rqm)
confint(resmob.rqm)
cor.test(D$ent_avg, resmob.rqm$residuals)
cor.test(D$ent_avg^2, resmob.rqm$residuals)
### no correlations between the predictors and residuals - good!
vif(resmob.rqm)
### correlations inflates standard errors of entropy coefficients only very lowly

### Wald test for nested models
anova(lmrob(resmob ~ 1, data=D), resmob.rlm)
anova(resmob.rlm, resmob.rqm)

### So it seems that the hypothesis holds in the case of the RESMOB scale.
### There is an association with average entropy and the quadratic model is better than the linear one
### Robust consistency corrected R^2 is 5.06%

### Model visualization
### Trellis device
trellis.device(color=FALSE, new=FALSE)
### graph
xyplot(resmob ~ ent_avg, data=D, alpha=.7, pch=1, col="black",
       fit = coef(resmob.rqm),
       jitter.data=TRUE, cex=1, type="p", grid=TRUE,
       EH = mean(D$ent_avg),
       xlab = "Średnia entropia (H)", ylab = "Mobilizacja Zasobów (MB)",
       panel = function(x, y, ..., fit, EH) {
             panel.xyplot(x, y, ...)
             panel.curve(fit[1]+fit[2]*x+fit[3]*(x-EH)^2,
                         lty=2, lwd=2.5, col="red")
             panel.abline(v=1.133359, lwd=.9, col="red")
             panel.rug(x = x, y = NULL)
             panel.text(0.25, 10.4, rot=-30,
                        expression(
                              paste(bold(MB), "= 9,23 - 1,44", bold(H),
                                    " - 3,69", bold(H)^2)))
       })

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
soccont.lm <- lm(soccont ~ ent_avg, data=D)
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
soccont.qm <- lm(soccont ~ ent_avg + I(ent_avg^2), data=D)
# Some diagnostics
# preliminary global test
gvlma(soccont.qm) # nice no deviations from the LM assumptions
qqPlot(soccont.qm$residuals)
shapiro.test(soccont.qm$residuals) # normal noise; good
bptest(soccont.qm) # homoscedastic
# No autocorrelations
dwtest(resmob.qm) # no autocorrelations
# Outliers
outlierTest(resmob.qm)

### I use robust bi-square estimation (95% of efficiency of OLS regression)
### In order to make the model compatible with the RESMOB model
### If not for that, there would be no reason to do this
### Models with bi-square robust weights
### Linear model
set.seed(111)
soccont.rlm <- lmrob(soccont ~ ent_avg, data=D)
Anova(soccont.rlm)
qqPlot(soccont.rlm$residuals)
shapiro.test(soccont.rlm$residuals)
outlierStats(soccont.rlm)

### Quadratic model
set.seed(111000)
soccont.rqm <- lmrob(soccont ~ ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
Anova(soccont.rqm)
qqPlot(soccont.rqm$residuals)
shapiro.test(soccont.rqm$residuals)
outlierStats(soccont.rqm)
confint(soccont.rqm)
cor.test(D$ent_avg, soccont.rqm$residuals)
cor.test(D$ent_avg^2, soccont.rqm$residuals)
### no correlations between the predictors and residuals - good!
vif(soccont.rqm)
### correlations inflates standard errors of entropy coefficients only very lowly

### Wald test for nested models
anova(soccont.rlm, soccont.rqm)

### Wald test for nested models
anova(lmrob(soccont ~ 1, data=D), soccont.rlm)
anova(soccont.rlm, soccont.rqm)

### So it seems that the hypothesis holds in the case of the SOCCONT scale.
### There is an association with average entropy and the quadratic model is better than the linear one
### Robust consistency corrected R^2 is 6,46%

### Model visualization
### graph
xyplot(soccont ~ ent_avg, data=D, alpha=.7, col="black", pch=1,
       fit = coef(soccont.rqm),  grid=TRUE,
       jitter.data=TRUE, cex=1, type="p", EH = mean(D$ent_avg),
       xlab = "Średnia entropia (H)", ylab = "Kontakty Społeczne (KS)",
       panel = function(x, y, ..., fit, EH) {
             panel.xyplot(x, y, ...)
             panel.curve(fit[1]+fit[2]*x+fit[3]*(x-EH)^2,
                         lty=2, lwd=2.5, col="red")
             panel.abline(v=0.87225, lwd=.9, col="red")
             panel.rug(x = x, y = NULL)
             panel.text(0.25, 10.4, rot=-30,
                        expression(
                              paste(bold(KS), "= 11,20 - 1,32", bold(H),
                                    " - 5,98", bold(H)^2)))
       })


### Summing up: Hypothesis I is not rejected.

### In the end I check models with number of indicated places as an additional predictor.
### The aim is to show that neither cluster assignment and places can not cover the quadratic relationship between entropy and social capital.


### Some correlations
lowerCor(D[, c("resmob", "soccont", "ent_avg", "places")])
### RESMOB
set.seed(909)
resmob.rqmex <- lmrob(resmob ~ places + cluster + ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
resmob.rqmexbase <- lmrob(resmob ~ places + cluster, data=D)
resmob.rqmexlin <- lmrob(resmob ~ places + cluster + ent_avg, data=D)
Anova(resmob.rqmex)
anova(resmob.rqmex, resmob.rqmexlin)
anova(resmob.rqmex, resmob.rqmexbase)
anova(resmob.rqmexlin, resmob.rqmexbase)
vif(resmob.rqmex)

### SOCCONT
set.seed(9090)
soccont.rqmex <- lmrob(soccont ~ places + cluster + ent_avg + I((ent_avg-mean(ent_avg))^2), data=D)
soccont.rqmexbase <- lmrob(soccont ~ places + cluster, data=D)
soccont.rqmexlin <- lmrob(soccont ~ places + cluster + ent_avg, data=D)
Anova(soccont.rqmex)
anova(soccont.rqmex, soccont.rqmexlin)
anova(soccont.rqmex, soccont.rqmexbase)
anova(soccont.rqmexlin, soccont.rqmexbase)
vif(soccont.rqmex)


### Difference between the maxima
### In the end I use bootstrap to find the confidence interval of the difference between the argmaxes of the two models.
### I make 1000 pairs of models sampling from the empirical distribution
### and compute differences rm.argmax - sm.argmax

### Computations
n = 1000    # number of bootstrap models
EH = mean(D$ent_avg)
Emax = entropy(c(1,2,3))
rm.maxima = vector(mode="numeric", length=n)
sc.maxima = vector(mode="numeric", length=n)

### Bootstrap loop
for(i in 1:n) {
      sD = sample_n(D, size=nrow(D), replace=TRUE)
      rm.mod = lmrob(resmob ~ ent_avg + I((ent_avg - mean(ent_avg))^2), data=sD)
      rm.cfs = coef(rm.mod)
      rm.axm = EH - rm.cfs[2] / (2*rm.cfs[3])
      if(rm.axm < 0 || rm.axm > Emax) rm.maxima[i] = NA
      else rm.maxima[i] = rm.axm
      sc.mod = lmrob(soccont ~ ent_avg + I((ent_avg - mean(ent_avg))^2), data=sD)
      sc.cfs = coef(sc.mod)
      sc.axm = EH - sc.cfs[2] / (2*sc.cfs[3])
      if(sc.axm < 0 || sc.axm > Emax || is.na(rm.maxima[i])) sc.maxima[i] = NA
      else sc.maxima[i] = sc.axm
}

### 95% CI for the difference
quantile(rm.maxima - sc.maxima, probs=c(.025,.975), na.rm=TRUE)
mean(rm.maxima - sc.maxima, na.rm=TRUE)