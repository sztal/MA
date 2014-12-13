### This is the script for scaling social capital indicators (Resource Generator)
### According the the best practices, nonparametric IRT mokken scaling is used
### The scaling procedure begins with the joint set of scknow (1-15) and schelp(1-15)
### and tries to determine what unidimensional scales may be derived from this set
library(mokken)
library(lattice)
source("Scaling/scaling-Helper.R")
source("HelperFunctionsMisc/ComputingMisc.R")

load("MainData/MainData2.RData") # loads the main dataset D
D.back = D # backup full dataset
D = D[,which(names(D)=="scknow1"):which(names(D)=="schelp15")] # only social capital

# Sort columns by popularity
D = meanSort(D)
popularity = apply(D, 2, mean)

# Item selection - genetic algorithm
scale.ga = aisp(D, search="ga", lowerbound=.4)
# items comprising the first dimension (the strongest one)
dim1 = rownames(scale.ga)[which(scale.ga==1)]
# remainding items
rem = names(D)[!names(D) %in% dim1]

# Check monotone homogeneity and double monotonicity for dim1
D.dim1 = D[, dim1]

# Latent monotonicity
mono.dim1 = check.monotonicity(D.dim1) # perfect fit to monotone homogeneity

# Nonintersection
# pmatrix
pmat.dim1 = check.pmatrix(D.dim1) # many violations, but very few relative to maximum
# restscore
rest.dim1 = check.restscore(D.dim1) # few vilations; none is significant
# These two methods are not really suitable for polytomous data
# so the main test is provided by invariant item ordering procedure (check.iio)
iio.dim1 = check.iio(D.dim1) # no violations; high HT value (0.49)
# so the scale conforms very well to the double monotonicity model

# Check unidimensionality
scale.ga.dim1 = aisp(D.dim1, search="ga")
# Clear unidimensional structure

# Scaling parameters
H.dim1 = coefH(D.dim1) # good global H; 95% CI for H: [0.36 - 0.54]
# Moreover, all Hi > 0.40
Hij.dim1 = coefH(D.dim1, nice.output=FALSE)$Hij # matrix of Hij
negative.dim1 = sum(Hij.dim1 < 0) # no negative Hij
# So the scale goodness-of-fit is satisfactory

# Reliability
rel.dim1 = check.reliability(D.dim1, LCRC=TRUE)
# MS = 0.85; alpha = 0.84; lambda-2 = 0.84; LCRC = 0.86
# Since the scale was derived from a potentially multidimensional item bank
# LCRC is a preferred reliability measure
# Nevetheless all of them indicate good measurement reliability and precision

### Let dim1 be called resource mobilization scale (resmob) ###

# Scaling for the remaining items
D.rem = D[, rem]

# Selection procedure - genetic algorithm
scale.ga.rem = aisp(D.rem, search="ga", lowerbound=.3) # with lower lowerbound this time
dim2 = rownames(scale.ga.rem)[which(scale.ga.rem==1)]
# only items from scknow
# it seems that this set of items correspond to knowing different,
# potenatially, important persons

# Check monotone homogeneity and double monotonicity for dim2
D.dim2 = D[,dim2]

# Latent monotonicity
mono.dim2 = check.monotonicity(D.dim2) # no violations
# The scale confomorms to the monotone homogeneity model

# Nonintersection
# pmatrix
pmat.dim2 = check.pmatrix(D.dim2) # some violations, but relatively not so many
# restscore
rest.dim2 = check.restscore(D.dim2) # again some violations, especially for scknow9
# But the main test is provided by check.iio since the items are polytomous
iio.dim2 = check.iio(D.dim2) # no significant violations
# however previous violations and high crit value for scknow9 suggest it should be removed

# The same proceedure but now without scknow9
D.dim2 = D.dim2[, -which(names(D.dim2)=="scknow9")]
dim2 = dim2[-which(dim2=="scknow9")]

# Latent monotonicity
mono.dim2 = check.monotonicity(D.dim2) # no violations
# The scale conforms to the monotone homogeneity model

# Nonintersection
# pmatrix
pmat.dim2 = check.pmatrix(D.dim2) # some violations, but relatively not so many
# restscore
rest.dim2 = check.restscore(D.dim2) # some violations, but none is significant
# Again, the main test is provided by check.iio since the items are polytomous
iio.dim2 = check.iio(D.dim2) # no vioaltions; moderate HT value (0.38)
# So the scale conforms to the model of double monotonicity quite well

# Scaling parameters
H.dim2 = coefH(D.dim2) # satisfactory H; 95% CI for H: [0.31 - 0.46]
# All Hi > 0.30
Hij.dim2 = coefH(D.dim2, nice.output=FALSE)$Hij
negative.dim2 = sum(Hij.dim2 < 0) # and no Hij < 0
# So the scale has satisfactory goodness-of-fit

# But item scknow 2 has low Hi, so it should be removed.

# Check the scale without scknow2
D.dim2 = D.dim2[, - which(names(D.dim2)=="scknow2")]

# Check monotonous homogeneity
mono.dim2 = check.monotonicity(D.dim2)
# Perfect

# Check double monotonicity
iio.dim2 = check.iio(D.dim2)
# Perfect - no violations

# Scalability coefficients
H.dim2 = coefH(D.dim2) # very good coefficients

# Check unidimensionality
scale.ga.dim2 = aisp(D.dim2, search="ga")
# Clearr unidimensional structure

# Reliability
rel.dim2 = check.reliability(D.dim2, LCRC = TRUE)
# MS = 0.78; alpha = 0.77; lambda-2 = 0.78; LCRC = 0.79
# SLightly lower reliability than dim2 but still good
# Again, LCRC should be considered the most important measure
# since the items were drawn from a potentially multidimensional item bank

### let dim2 be called social contacts scale (soccont) ###

# Compute the scales
resmob = apply(D.dim1, 1, sum)
soccont = apply(D.dim2, 1, sum)

# Distributions of the scales
hist(resmob) # it is strongly leftskewed
# Distributions of items of socmob
histogram(expr(D.dim1), data=D.dim1, xlab="", layout=c(5,2))
# The histograms show that the majority of items is also very leftskewed,
# it should come as no surprise that the scale is skewed as well
hist(soccont) # more balanced histogram, althought still slightly skewed
# Distributions of items of soccont
histogram(expr(D.dim2), data=D.dim2, xlab="", layout=c(4,2))
# here the distributions are also more balanced

# Add the scales to the dataset
D.back$resmob = resmob
D.back$soccont = soccont
# Histograms of the scales
histogram(~ resmob + soccont, data=D.back, xlab="") # skewed, - do not differentiate well between people with high social capital

# Check if the rest of items form some reasonable scale
rem = names(D)[!names(D) %in% c(names(D.dim1), names(D.dim2))] # remaining itrms
D.rem = D[, rem]

# genetic algorithm
scale.ga.rem = aisp(D.rem, search="aisp", lowerbound=.3)
# Clearly there is no more clear unidimensional structures within the data

# Save the new dataset
D = D.back
save(D, file="MainData/MainData3.RData")

# saving workspace image (genetic algorithms are computationally expensive)
save.image("Scaling/Scal-ResGenAll.RData")
# saving the scales' item banks for further analyses
save(list=c("D.dim1", "D.dim2"), file="Scaling/resmob_soccont_itembanks.RData")
rm(list=ls())