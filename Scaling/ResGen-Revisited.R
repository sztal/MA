# This script is dedicated to finding better scales of social capital that can well differentiate between people with high capital
library(mokken)
library(lattice)
library(psych)
source("Scaling/scaling-Helper.R")
source("HelperFunctionsMisc/ComputingMisc.R")

# Load the workspac image from "Scaling-ResourceGenerator.R"
load("Scaling/Scal-ResGenAll.RData")
load("MainData/MainData10.RData")

D10 = D
scale.ga.dim1.v2 = aisp(D.dim1, search="ga", lowerbound=.5)
dim1.v2 = rownames(scale.ga.dim1.v2)[scale.ga.dim1.v2[,1] == 1]
D.dim1 = meanSort(D.dim1)
D.dim1.v2 = D.dim1[,dim1.v]
# The procedure selected only ease items. This is not the way to go.

diffs.dim1.v2 = apply(D.dim1.v2, 2, mean)
diffs.dim1 = apply(D.dim1, 2, mean)

D.help = D.back[, grep('schelp', names(D.back))]
diffs.help = apply(D.help, 2, mean)

# Get schelp variables with easiness (mean) lower than 3.
D.hdiff = D.help[, names(diffs.help)[diffs.help <= 3]]
# Check scaling of this dataset
# Monotonous homogeneity
mono.hdiff = check.monotonicity(D.hdiff) # quite ok
# Double monotonicity
# Invariant item ordering
iio.hdiff = check.iio(D.hdiff) # no violations, but very low HT?
# Rest scores
rest.hdiff = check.restscore(D.hdiff) # no significant violations even here, so maybe it is okay?

# Scaling coefficients
H.hdiff = coefH(D.hdiff) # acceptable scaling

# Automatic item selection proceure
scale.ga.hdiff = aisp(D.hdiff, search="ga", lowerbound=.35)
# Item corresponding to health advice is rejected; it makes sense since it may be a bit random thing; moreover this is almost the most easy item in this set
D.hdiff = D.hdiff[, rownames(scale.ga.hdiff)[scale.ga.hdiff[,1] == 1]]

# Scaling again
# Monotonous homogeneity
mono.hdiff = check.monotonicity(D.hdiff)
# Double monotonicity
# Invariant item ordering
iio.hdiff = check.iio(D.hdiff) # HT is still low, but lets check other measures
# Rest scores
rest.hdiff = check.restscore(D.hdiff) # no significant violations
# Pmatrices
pmat.hdiff = check.pmatrix(D.hdiff) # very well - no significant violations even in regard to P matrices!
# Scallability coefficients
H.hdiff = coefH(D.hdiff) # quite nice scallability
# Reliability
Rel.hdiff = check.reliability(D.hdiff, LCRC = TRUE) # acceptable
# Check the distribution
resmob = apply(D.hdiff, 1, sum)
histogram(resmob) # the distribution looks a bit better

# Get scknow variables with low easiness (means)
D.kdiff = D.back[, grep("scknow", names(D.back))]
diff.know = apply(D.kdiff, 2, mean)
D.kdiff = D.kdiff[, names(diff.know)[diff.know <= 3]]

# Scaling
# Montonous homogeneity
mono.kdiff = check.monotonicity(D.kdiff) # Very low Hi
# Automatic selection is needed
scale.ga.kdiff = aisp(D.kdiff, search="ga", lowerbound=.35)
D.kdiff = D.kdiff[, rownames(scale.ga.kdiff)[scale.ga.kdiff[,1]==1]] # very nice and quite even distribution of item difficulties
# Scaling again
# Monotonous homogeneity
mono.kdiff = check.monotonicity(D.kdiff) # coefficients are ok, but not good
# Double monotonicity
# Invariant item ordering
iio.kdiff = check.iio(D.kdiff) # nice, but HT is low for some reason again
# Rest scores
rest.kdiff = check.restscore(D.kdiff) # no significant violations
# P matrices
pmat.kdiff = check.pmatrix(D.kdiff) # no violations!
# Scallability coefficients
H.kdiff = coefH(D.kdiff) # on the verge of being poor, but it seems there is nothing that can be done about it :(
# Reliability
Rel.kdiff = check.reliability(D.kdiff, LCRC = TRUE) # poor but still acceptable
# Distribution
soccont = apply(D.kdiff, 1, sum)
histogram(soccont) # but the sitribution is very nice!

# Save the new versions of the scales
D$resmob = resmob
D$soccont = soccont

# Save the dataset
save(D, file="MainData/MainData11.RData")

rm(list=ls())

# This is it folks!