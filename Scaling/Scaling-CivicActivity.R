### This script describes and replicates a scaling procedure
### that has been used to scale data on social/civic activity
### variables: from socact1 to socact14
### I employ mokken scaling (and use "mokken" package)
library(mokken)
source("Scaling/scaling-Helper.R")

load("MainData/MainData1.RData") # load the main dataset
D.back = D # backup copy of the full dataset
D = D[,which(names(D)=="socact1"):which(names(D)=="socact14")]

# sort columns by frequency distribution
D = meanSort(D)
freq = apply(D, 2, mean) # frequency distributions
Zeros = apply(D, 1, sum) # some respondents with no variation (zero response patterns)

# Check latent monotonicity
mono = check.monotonicity(D) # no violations of monotonicity
# althought socact14 seems to scale poorly

# Nonintersection
# Pmatrix method
pmat = check.pmatrix(D) # some violations; again socact14 scale poorly
# Restscore method
rest = check.restscore(D) # again socact14 scales very poorly

# Invariant Item Ordering
iio = check.iio(D, method="MIIO") # some violations; socact14 scales poorly

# all three methods indicate that socact14 should be excluded from the scale

# Scale
H = coefH(D) # overal scaling is quite ok, but socact14 needs to be removed
# automated item selection
scale = data.frame(scale.h = aisp(D), scale.ga = aisp(D, search="ga"))
names(scale) = c("scale.h", "scale.ga")
# Genetic algorithm also shows that socact14 is not a part of the dimension

# Scaling without socact14
D = D[, -which(names(D)=="socact14")]

# latent monotonicity
mono = check.monotonicity(D) # Latent monotonicity holds without exceptions

# nonintersection
# Pmatrix
pmat = check.pmatrix(D) # some slight violations - nothing important
# restscore
rest = check.restscore(D)
# Intersection is only slightly violated; no violation is significant

# invariant item ordering
iio = check.iio(D) # no significant violations
# HT (> 0.5) indicates strong scale

# scaling
H = coefH(D) # 95% CI for H is [0.3342 - 0.5498] - so the scale is relatively good
scale = data.frame(aisp(D), aisp(D, search="ga"))
names(scale) = c("scale.h", "scale.ga") # both algorithms yield the same solution
# The solution is purely unidimensional

# reliability
rel = check.reliability(D) # quite good reliability measures

# Guttman errors
errors = check.errors(D)
# reltive errors
rel_err = errors / gutmax(13) # guttman errors for persons are low too

# So the final scale of social / civic activity is composed of socact1 to socact13
civic = apply(D, 1, sum)

# Check scale without respondent with no variation
Zeros = apply(D, 1, sum)
novar = which(Zeros==0)
D.nv = D[-novar,]

# Scale fit for D.nv
H.nv = coefH(D.nv) # a bit worse scale
# However it is not entirely clear whether in the scale with difficult item
# (such as this one) one should remove no variation observations
# I choose not to do so

# Back to the main dataset
D = D.back
D$civic = civic

# Save the dataset
save(D, file="MainData/MainData2.RData")

rm(list=ls())
