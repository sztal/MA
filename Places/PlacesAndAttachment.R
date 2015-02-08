# This script produces poisson regression model for predicting the number of places indicated by repondents on the basis of their place attachment.
# As it will turn out, eventually the negative binomial model will be used, since the data are overdispersed

# Necessary packages and data etc.
library(psych)
library(lattice)
library(latticeExtra)
library(car)
library(MASS)
# Load the main dataset
load("MainData/MainData14.RData")
D.back = D # full data backup

# Preliminary correlations between number of indicated places and attachment scales
lowerCor(D[,c(76, 40:43)])
# again, clearly only the general scale and the place discovered are relevant here

# Important model checking in order to be sure that poisson regression may be applied
places <- D$places
c(mean(places), var(places)) # rather not poissonion thing :/
h1 <- histogram(places, main="", xlab="Ilość wskazanych miejsc")
my.theme <- trellis.par.get()
my.theme$strip.background$col <- "grey80"
my.theme$plot.symbol$pch <- 16
my.theme$plot.symbol$size <- 4
my.theme$plot.symbol$col <- "grey60"
my.theme$plot.polygon$col <- "grey10"
h1.gray <- update(h1, par.settings = my.theme)

# So I use the negative binomial model
# First I test a model with the general scale of attachment
nbm1 <- glm.nb(places ~ attgen, data=D)
fit1 <- nbm$fitted
# Now the place discovered
nbm2 <- glm.nb(places ~ attdiscovered, data=D)
fit2 <- nbm2$fitted
# Now both together
nbm3 <- glm.nb(places ~ attgen + attdiscovered, data=D)
fit3 <- nbm3$fitted
# LR test
nbm0 <- glm.nb(places ~ 1, data=D)
anova(nbm0, nbm1)