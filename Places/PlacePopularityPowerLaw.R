####################################################################
###   Power-law distribution test for popularity of the places   ###
####################################################################
library(igraph)
library(lattice)
library(latticeExtra)
source("HelperFunctionsMisc/ComputingMisc.R")
source("Networks/NetworkMethods.R")


# This script test the hypothesis that the distribution of place popularities is scale-free

load("Places/PlaceData.RData")

# Frequency distribution of popularity
pop_dist = prop.table(table(Pdat$popularity))
# Check the distribution histogram
histogram(~ Pdat$popularity, xlab="Popularity") # very power-lawish...
# logged probabilities and popularities (base 10)
log_pop_pr = as.numeric(log10(pop_dist)) # logged probabilities
log_pop = log10(as.numeric(names(pop_dist))) # logged popularities
# Double log-scale plot
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
alpha.mle = fit$alpha
# Now compute the constant (log of it is the constant in the mle regresion model)
C = (1 - alpha.mle) / (-1) # this can be derived from the fact that propability density function integral has to equal to 1

# Nice plot!
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
# Very nice power-law!
# So summing up: there is a power-law to the distribution of the popularity of places!

