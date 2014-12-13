# This script computes reliability measures and scaling for place attachments scales
# We begin with the unidimensional scale (attgen1 to attgen9)
library(psych)
library(GPArotation)
library(doBy)
library(lattice)

load("MainData/MainData3.RData") # loads an object D - the main dataset
D.back = D # Full backup dataset
D = D[,which(names(D)=="attgen1"):which(names(D)=="attgen9")]

# Scaling - reliability and error bars
alpha(D) # very good classical unidimensional scaling (Cronbach's alpha = 0.89)
# but two items need to be reversed
lowerCor(D) # these variables are attgen2 and attgen3
# recode the reversed variables
for(i in 2:3) D[,i] = recodeVar(D[,i], c(1,2,3,4,5), c(5,4,3,2,1))
# Now everything is fine and the scale ha alpha = 0.89
# Add the scale
D.back$attgen = apply(D, 1, sum)
# Distribution of the scale and the items
histogram(~attgen, data=D.back, xlab="Przywiązanie do miejsca") # the distribution is slightly left-skewed
# item distributions
histogram(~ attgen1+attgen2+attgen3+attgen4+attgen5+attgen6+attgen7+attgen8+attgen9, data=D, xlab="", layout=c(3,3))


# Now for the 3-dimensional scale
D = D.back[,which(names(D.back)=="attmult1"):which(names(D.back)=="attmult18")]
# Factors are based on the results of Lewicka (2012)
# The first factor "the place given"
# items (attmult): 5, 10, 11, 15, 8, 7
place.given = names(D)[c(5,10,11,15,8,7)]
D.place.given = D[, place.given]
# Correlation matrix to inspect reversed items
lowerCor(D.place.given) # attmult11 should be reversed
D.place.given[,"attmult11"] = recodeVar(D[,"attmult11"], c(1,2,3,4,5), c(5,4,3,2,1))
alpha(D.place.given) # poor scaling: aplha = 0.59
# items' distributions
histogram(~attmult5+attmult10+attmult11+attmult15+attmult8+attmult7, data=D.place.given, xlab="") # most of the items is slightly skewed - maybe this is the reason for the poor realibility?
# Maybe the sample has different structure?
# Let's check relaibilities of the other scales

# The second dimension "the palce discovered"
# items (attmult): 16, 6, 3, 9, 14, 4
place.discovered = names(D)[c(16,6,3,9,14,4)]
D.place.discovered = D[, place.discovered]
# Correlation matrix to inspect reversed items
lowerCor(D.place.discovered) # it seems there is no reversed items
alpha(D.place.discovered) # satisfactory reliability: alpha = 0.77
# items' distributions
histogram(~attmult16+attmult6+attmult3+attmult9+attmult14+attmult4, data=D.place.discovered, xlab="") # the distributions are similiarily skewed as in the case of the first dimension

# The third dimension "no attachment"
# items (attmult): 1, 17, 13, 12, 18, 2
no.attachment = names(D)[c(1,17,13,12,18,2)]
D.no.attachment = D[, no.attachment]
# Correlation matrix to inspect reversed items
lowerCor(D.no.attachment) # it seems there is no reversed items
alpha(D.no.attachment) # satisfactory reliability: alpha = 0.76
# items' distributions
histogram(~attmult1+attmult17+attmult13+attmult12+attmult18+attmult2, data=D.no.attachment, xlab="") # Again, no substantial diffenece in regard to "the place given"

# Thus, there is no need for a new factor structure. Nevertheless maybe there is a chance to improve the scalability of the first dimension
# inspect the reliability again
alpha(D.place.given)
# it seems that dropping attmult11 may improve the scalability
D.place.given = D.place.given[, -which(names(D.place.given)=="attmult11")]
alpha(D.place.given)
# again, dropping attmult8 will improve the scale
D.place.given = D.place.given[, -which(names(D.place.given)=="attmult8")]
alpha(D.place.given)
# and again, dropping attmult7 will improve the scale (quite a lot!)
D.place.given = D.place.given[, -which(names(D.place.given)=="attmult7")]
alpha(D.place.given)
# and again, dropping attmult15 will substantially improve the scale
D.place.given = D.place.given[, -which(names(D.place.given)=="attmult15")]
alpha(D.place.given)
# now relaibility is good: alpha = 0.80

# It turns out that all the discarded items are questions that may seem not very appropriate for young adults, in a sense that they can be a bit anachronic for this particular age group (cohort). See the codebook for the description of the questions.
# The two other scales are optimal: there are no items that may be discarded in order to improve their reliabilities
# Moreover, after this rudimentary item selection all scales have comparable reliabilities what is desired, since they will be often analyzed jointly

# Compute the scales (as means in order to get them to the same range)
attgiven = apply(D.place.given, 1, mean)
attdiscovered = apply(D.place.discovered, 1, mean)
attnoatt = apply(D.no.attachment, 1, mean)

# put them into the main dataset
D.back$attgiven = attgiven
D.back$attdiscovered = attdiscovered
D.back$attnoatt = attnoatt

# Check the scales' distributions
histogram(~attgiven+attdiscovered+attnoatt, data=D.back, layout=c(3,1), xlab="")
# attgiven is slightly right-skewd, but the other are nicely symmetric
# Interscales correlations
lowerCor(D.back[,183:189])

# Save the dataset
write.csv(D.back, file="MainData/MainData4.csv", row.names=FALSE)
D = D.back
save(D, file="MainData/MainData4.RData")
rm(list=ls())