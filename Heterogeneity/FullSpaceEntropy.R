# This script performs additional analyses related to social space heterogeneity and social capital. The general idea is to give some more insight into the real impact of heterogeneity. Since it was confirmed that average entropy of places is significantly related to social cpaital (and to some extent moderated by active place attachment) it is necessary to verify wheter the real factor here is the diversity of various places (social subspaces) or the social space as such. In order to study this relationship it is necessary to estimate entropy of the full social space. The full space entropy measure will be used for this purpose. Full space entropy is just an entropy of the distribution of all persons that can be met by a person through the places he or she visits. This is completely different variable than average place entropy, since average entropy tells a lot about how diversed are places by themselves, but not so much about the diversity of the entire social space of a person. Of course highly diverse places should be linked to large diveristy of the whole space bit the reverse is not necessaruly true, since many places may be not diversed at all but their joint distribution may be very dispersed.
library(psych)
library(lattice)
library(latticeExtra)
library(lmtest)
library(car)
library(MASS)
library(reshape2)
library(RColorBrew)
source("HelperFunctionsMisc/ComputingMisc.R")
source("Places/PlacesHelper.R")

# Load the dataset
load("MainData/MainData11.RData")
load("Networks/IncidenceMatrix.RData")

D$fullent = realEntropy(D, AM)

# save the dataset with a new varibale
save(D, file="MainData/MainData12.RData")

# Restrict the dataset only to relevan variables
D = D[, c(1, 38:43, 72, 77)]

# Analysis - correlation between average entropy and full space entropy
lowerCor(D[, 2:dim(D)[2]])

# Distributions of average entropy and full space entropy
histogram(~ fullent + ent_avg, data=D, xlab="")
# Full space entropy has clearly much less nice distribution

# Full space entropy is only moderately correlated with the average entropy
# Correlgrams for full space entopy and social capital
E = D[, c(1:3, 9)]
E = melt(E, id=c("id", "fullent"))
levels(E$variable) = c("Mobilizacja zasobów", "Kontakty społeczne")
# Plot
xyplot(value ~ fullent | variable, data=E, ylab="Kapitał społeczny", xlab="Pełna entropia przestrzeni",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       }) # terrible outliers

# Linear models for full spac entropy
D.back = D
D = D[!is.na(D$fullent), ]
resmob.lm1 = lm(resmob ~ fullent, data=D)

resmob.lm2 = lm(resmob ~ fullent + I(fullent^2), data=D)
# clearly quadratic effect is not important in this case
# In general real entropy is not suitable for linear models

soccont.lm1 = lm(soccont ~ fullent, data=D)
soccont.lm2 = lm(soccont ~ fullent + I(fullent^2), data=D) # nothing

# Lets how it looks like without zero entropy observations, since they are heavy outliers
D.nz = D[D$fullent != 0, ]

histogram(~ fullent + ent_avg, data=D.nz)
E.nz = D.nz[, c(1:3, 9)]
E.nz = melt(E.nz, id=c("id", "fullent"))
levels(E$variable) = c("Mobilizacja zasobów", "Kontakty społeczne")
xyplot(value ~ fullent | variable, data=E.nz, ylab="Kapitał społeczny", xlab="Pełna entropia przestrzeni",
       panel = function(x, y) {
             panel.xyplot(x, y)
             panel.abline(lm(y ~ x))
             panel.loess(x, y, col="red")
       })

resmob.lm1 = lm(resmob ~ fullent, data=D.nz)
resmob.lm2 = lm(resmob ~ fullent + I(fullent^2), data=D.nz) # nothing
soccont.lm1 = lm(soccont ~ fullent, data=D.nz)
soccont.lm2 = lm(soccont ~ fullent + I(fullent^2), data=D.nz) # nothing

# Impact of full space entropy on average entropy models
resmob.lm1 = lm(resmob ~ ent_avg + I(ent_avg^2) + fullent, data=D) # nothing, it is absolutely irrelevant

soccont.lm1 = lm(soccont ~ ent_avg + I(ent_avg^2) + fullent, data=D) # nothing, it is absolutely irrelevant