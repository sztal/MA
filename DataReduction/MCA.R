# This script performs MCA for categorical variables (demographic, socioeconomic and cultural / taste)
library(lattice)
library(psych)
library(ca)
source("DataReduction/MCAhelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")


load("MainData/MainData7.RData") # loads the dataset as an object D
D.back = D # full backup dataset

# First I scale cultural capital in classical understanding
# variables indicating ccc
vars = names(D.back)[c(5, 9:11, 20:30, 35:37)]
D = D.back[, vars]
cc.mca = mjca(D, nd=NA, lambda="adjusted") # adjusted lambda as recommended by Greenacre
# quite nice almost 62.5% of variance is retained in the first two dimensions

cc.ind.coord = projectInd(D, cc.mca) # gets coordinates for respondents
par(mar=c(4,4,1,1))
makeMCAplot(D, cc.mca, cex=.6, zoomout=1.2, xlab="Wymiar I", ylab="Wymiar II") # MCA plot

### Interpretation ###
# This is a very good solution - almost all categories are well reprooduced in two-dimensional space - moreover it has a very clear interpretation
# The first dimension (that reproduces 52.5% of inertia) seems to correspond to the classical definition of cultural capital that is generationally transmitted and is focused around consumption and usage of cultural goods
# The second dimension is just residual and will not be considered in further analyses 
# So respondens scores will be saved only for the first dimension which may be interpreted as general cultural capital

D.back$cultcap = cc.ind.coord[,1] # save the cultural capital scores
# Check distributions
histogram(~ cultcap, data=D.back, xlab="Kapitał kulturowy")
# nice symmetric distributions
# Check correlations with other scales / variables
lowerCor(D.back[,41:69]) # good; especially cultcap scale has theoretically meaningfull correlations
# Correlations with cultural preferences
lowerCor(D.back[,48:69])

# One-dimensional plot
ans = cc.mca$levelnames
coords = cc.mca$rowpcoord[,1]
dotplot(reorder(ans, coords) ~ coords, xlab="Kapitał kulturowy")


# Socioeconomics and demographics
D = D.back
vars = names(D.back)[c(2,6:8,14:18,39:40)]
D = D.back[, vars]

sed.mca = mjca(D, nd=NA, lambda="adjusted")
# Over 69% of inertia is explained by the first two dimensions
# almost all categories have very good qualities - only STEM/med education is reproduced poorly
sed.ind.coord = projectInd(D, sed.mca) # gets coordinates for respondents
par(mar=c(4,4,1,1))
makeMCAplot(D, sed.mca, cex=.6, zoomout=1.2, xlab="Wymiar I", ylab="Wymiar II") # MCA plot
par(mar=c(4,4,4,4))

### Interpretation ###
# Again, the interpretation is very clear
# The first dimension (52.9% of inertia) corresponds to the general life stability - having a good and stable job and a degree.
# The second dimension (15.5% of inertia) seems to correspond more speifically to a financial situation, that is the less and more valueable things in possession - especially car/cars and home ownership, so in general it corresponds to wealthiness.
# Note that the second dimension is reversed.

# So the scores for individuals may be saved as  lifestab and wealth
D.back$lifestab = sed.ind.coord[,1] * (-1)
D.back$wealth = sed.ind.coord[,2] * (-1)
# Check the distributions
histogram(~ lifestab + wealth, data=D.back, xlab="")
# wealth distributions is nicely symemtric, but the lifestab distributions is a bit weirdly double-peaked, but it should not be a problem
# Check the correlations with other scales etc.
lowerCor(D.back[,41:71])

# Get rid of unuseful variables
D = D.back
D = D[, -c(which(names(D)=="dog"), which(names(D)=="internetuse"))]
D = D[, -c(which(names(D)=="trainfreq"), which(names(D)=="trainfav"))]

# Save the dataset
save(D, file="MainData/MainData8.RData")

par(mar=c(4,4,4,4))
rm(list=ls())
# This is it folks!
