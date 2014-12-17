# This script perform exploration and testing of the hypothesis that social space heterogeneity should be positively related to social capital measures.
library(car)
library(MASS)
library(psych)
library(QuantPsyc)
library(lattice)
library(latticeExtra)
library(reshape2)
library(lmtest)
source("HelperFunctionsMisc/ComputingMisc.R")

# Load the data
load("MainData/MainData11.RData")
D.back = D # backup dataset
D = D.back[, c(1, 38:43, 68:72, 76)]
E = D[, c(1:3, 12)]
# normalize the data in order to present it on a common scale
E = melt(E, id=c("id", "ent_avg"))

# Scatterplots
xyplot(value ~ ent_avg | variable, data=E,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       }) # there are perhaps some outliers;

# Check correlation of ent_avg with places to see if it is important to control for it (and also check correlations with attachment scales)
lowerCor(D[,c(2, 12:13, 4:7)]) # no strong correlations

# Save square of ent_avg for testing models with quadratic term
D$ent_avg2 = D$ent_avg^2

# Distributions of social capital scales
histogram(~ resmob + soccont, data=D) # a bit limited but in general they look quite ok
# Distribution of the average entropy
histogram(~ ent_avg, data=D, xlab="Average entropy") # also quite ok
# None of the distributions is normal

# MODELS for resmob
# Bivariate model
resmob.lm1 = lm(resmob ~ ent_avg , data=D) # 2.5% of variance explained
# Some diagnostics
shapiro.test(resmob.lm1$residuals) # not normal distribution of residuals; something is wrong
bptest(resmob.lm1) # ok, the variance is homoscedastic; that is good
raintest(resmob.lm1) # and the relationship is linear; that is also good
# No autocorrelations
dwtest(resmob.lm1) # no autocorrelations
# Outliers
outlierTest(resmob.lm1) # it seems there is not strong outliers...

# Test interactions effects with attachments scales
# Attachment and resmob - correlations
lowerCor(D[c(2,4:7)]) # only attgen and attdiscovered are to some extent correlated

# Interaction model with general place attachment
resmob.gen = lm(resmob ~ ent_avg*attgen, data=D)
# Diagnostics
shapiro.test(resmob.gen$residuals)
bptest(resmob.gen)
raintest(resmob.gen)
dwtest(resmob.gen)
outlierTest(resmob.gen)
# But the general fit is poor, since the distributon is very not normal

# Interaction with place discovered
resmob.disc = lm(resmob ~ ent_avg*attdiscovered, data=D)
# no significatn interaction term
shapiro.test(resmob.disc$residuals)
bptest(resmob.disc) # heteroscedasticity problem
raintest(resmob.disc)
dwtest(resmob.disc)
outlierTest(resmob.disc)


# Soccont
soccont.lm1 = lm(soccont ~ ent_avg, data=D)
# non-significant
# model with the quadratic term
soccont.lm2 = lm(soccont ~ ent_avg + I(ent_avg^2), data=D)
# Diagnostics
shapiro.test(soccont.lm1$residuals) # it is normal!
bptest(soccont.lm1) # homoscedastic!
dwtest(soccont.lm1) # but there is some autocorrelation for some reason...
raintest(soccont.lm1) # linear
outlierTest(resmob.lm1) # no significant outliers (with Bonferroni correction)

# Visualization
xyplot(soccont ~ ent_avg, data=D,
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.loess(x, y, col="red")
             panel.curve(6.645+8.004*x-4.999*x^2)
       })

# Interactions with attgen and attdiscovered
soccont.gen = lm(soccont ~ ent_avg*attgen + I(ent_avg^2), data=D)
# no significant interactions
# Interactions with attdiscovered
soccont.disc = lm(soccont ~ ent_avg*attdiscovered + I(ent_avg^2), data=D)
# no significant interaction