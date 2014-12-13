# This script does data reduction manipulations (mainly pca's) on the batteries of the (quasi-)continuous indicators of cultural preferences (tv preferences etc.)
library(psych)
library(doBy)
library(lattice)
library(GPArotation)
source("HelperFunctionsMisc/ComputingMisc.R")

load("MainData/MainData4.RData") # load the main dataset D
D.back = D # Full backup dataset

# Note: everytime quartimin rotation will be used since it is an oblique rotation that gives the most clear loadings pattern. The whole point of this data reduction procedure is to prepare the data for a cluster analysis, so dimensions have to be reduced in order to lower the impact of the curse of dimensionality, and at the same time the correlation patterns should be preserved. Therefore an oblique rotation is the best choice.
# Moreover, I use PCA instead of FA because I am not interested specifically in common factors between the variables but mainly in getting the data reduced and only after that in common patterns.
# In order not to distort th data I will use factor scores for observations for the new scales' values. It will also preserve the inter-factor correlation structure.

# First we deal with tv preferences
D = D[,which(names(D)=="tv.a1"):which(names(D)=="tv.b9")]
# variables' distributions
histogram(expr(D), data=D, xlab="", layout=c(6,3))
lowerCor(D)
# There is variation and the distributions are not too 'extreme'
# Data reduction - PCA
pca.tv = principal(D, nfactors=10, rotate="quartimax")
# data reduction does not work well here - there are no clear correlation patterns

# Let's try tv.a and tv.b separately

# tv.a - programs and channels' types
D.a = D[,which(names(D)=="tv.a1"):which(names(D)=="tv.a8")]
# Distributions
histogram(expr(D.a), data=D.a, xlab="")
pairs.panels(D.a)
cor.tv.a = cor(D.a) # correlation matrix
fa.parallel(D.a) # parallel monte carlo analysis suggests 3 components
pca.tv.a = principal(D.a, nfactors=3, rotate="quartimin")
# Three dimensions retain 64% of variance
pca.tv.a = fa.sort(pca.tv.a)
factor.fit(cor.tv.a, pca.tv.a$loadings) # fit: 0.81

# Sport news has very low communality, so  it is discarded and the analysis is reapeated.
D.a = D.a[, -which(names(D.a)=="tv.a2")]
cor.tv.a = cor(D.a) # correlation matrix
fa.parallel(D.a) # parallel monte carlo analysis suggests 3 components
pca.tv.a = principal(D.a, nfactors=3, rotate="quartimin")
# Three dimensions retain 70% of variance
pca.tv.a = fa.sort(pca.tv.a) # fit: 0.81

# Interpretation:
# dim1 (24%) : tv.a5, tv.a3 (local news and general news) : this is clearly an information focuse type of tv preferences.
# dim2 (25%) : tv.a6, tv.a1, tv.a4 (reality shows, entertainment, sitcoms) : this is clearly a more mainstream / lowbrow tv taste
# dim3 (21%) : tv.a7, tv.a8 (films, education and cultural tv) : and this is obviously a more highbrow taste.

# Moreover all communalities are rather high (above 0.6) so the solution is of high quality.

# Saving scales' scores
D.back$tvinfo = pca.tv.a$scores[,1]
D.back$tvlowbrow = pca.tv.a$scores[,2]
D.back$tvhighbrow = pca.tv.a$scores[,3]


# tv.b - polish tv channels
D.b = D[, which(names(D)=="tv.b1"):which(names(D)=="tv.b9")]
# Distributions
histogram(expr(D.b), data=D.b, xlab="")
pairs.panels(D.b)
cor.tv.b = cor(D.b) # correlation matrix
fa.parallel(D.b) # parallel monte carlo analysis suggests 2 components
pca.tv.b = principal(D.b, nfactors=2, rotate="quartimin")
# Two dimensions retain 53% of variance
factor.fit(cor.tv.b, pca.tv.b$loadings) # fit: 0.86 or 0.79 - good
pca.tv.b = fa.sort(pca.tv.b)

### Interpretation ###
# dim1 - tv.b2, tv.b5, tv.b3, tv.b1, tv.b8 (TVN, TVN24, Polsat, TVP, TVN Style)
# This dimension clearly correspond to a typical-popular profile of a polish tv watcher. It is loaded only by the country-wide big (state-owned and commercial) tv stattions.

# dim2 - tv.b4, tv.b6, tv.b9, tv.b7 (TV Trwam, TV Republika, Polsat Sport, TVP Historia)
# This dimension has a little bit more complicated meaning. First of all it is strongly loaded by two of the most important conservative tv stations (TV Republika and TV Trwam). Simultaneoulsy it is loaded by one of the main sport channels and the main national historic channels. Thus, it seems that it corresponds to a group of viewers that has predominantly conservative taste and views and tends to prefer sport / historic tv over mainstream popular tv entertainment.

# It is worth noting that the factors are (almost) orthogonal!

# Saving scales' scores
D.back$tvpop = pca.tv.b$scores[,1]
D.back$tvcons = pca.tv.b$scores[,2]

# Correlations between tv.a and tv.b scales
lowerCor(D.back[190:194])


# Taste for movies and cinema
D = D.back[, which(names(D.back)=="movie.a1"):which(names(D.back)=="movie.a10")]
# Distributions
histogram(expr(D), data=D, xlab="")
pairs.panels(D)
cor.movies = cor(D) # correlation matrix
fa.parallel(D) # parallel monte carlo analysis suggests 3 components
pca.movies = principal(D, nfactors=3, rotate="quartimin")
# Three dimensions retain 55% of variance
factor.fit(cor.movies, pca.movies$loadings) # fit: 0.74 - moderately good
pca.movies = fa.sort(pca.movies)
# movie.a8 (criminals / cinema noir) has very low communality (h = 0.21)
# Repeat analysis without this item
D = D[,-which(names(D)=="movie.a8")]
# Distributions
histogram(expr(D), data=D, xlab="")
pairs.panels(D)
cor.movies = cor(D) # correlation matrix
fa.parallel(D) # parallel monte carlo analysis suggests 3 components
pca.movies = principal(D, nfactors=3, rotate="quartimin")
# Three dimensions retain 60% of variance
factor.fit(cor.movies, pca.movies$loadings) # fit: 0.79 or 0.78 - moderately good
pca.movies = fa.sort(pca.movies) # now communalities are good

### Interpretation ###
# dim1 (19%) : movie.a5, movie.10, movie.a2, movie.a4 (fantasy / sci-fi, war movies, animations, historic / costume movies) : this factor seems to correspond to more alternative taste that revolves around fantasy and sci-fi and other epic narratives.s

# dim2 (20%) : movie.a3, movie.a1, movie.a4, movie.a7 (arthouse, drama, historic / costume movies, musicals) : this dimensions corresponds to more highbrow taste, but perhaps not exactly. First of all it contains musicals which are not so easy to classify. Nevertheless it seems that this factor is the closest one to a highbrow cinema taste.

# dim3 (20%) : tv.a6, tv.a9 (romantic comedies, comedies)
# This factor seems to correspond to light and rather lowbrow taste that prefers romantic comedies and comedies.

# Saving scales' scores
D.back$moviesepic = pca.movies$scores[,1]
D.back$movieshigh = pca.movies$scores[,2]
D.back$movieslight = pca.movies$scores[,3]

# Correlations with tv scales
lowerCor(D.back[,190:197])

# Taste for books and literature
D = D.back[,which(names(D.back)=="book.a1"):which(names(D.back)=="book.a10")]
# Distributions
histogram(expr(D), data=D, xlab="")
pairs.panels(D)
cor.books = cor(D) # correlation matrix
fa.parallel(D) # parallel monte carlo analysis suggests 3 components
pca.books = principal(D, nfactors=3, rotate="quartimin")
# Three dimensions retain 52% of variance and the fit is poor
factor.fit(cor.books, pca.books$loadings) # fit: 0.67 or 0.71 - poor
pca.books = fa.sort(pca.books) # factor loadings do not have a simple structure
# Moreover there are several low communalities
# Repeat analysis with 4 factors
# This is because parallel test almost indicated 4 factors
pca.books = principal(D, nfactors=4, rotate="quartimin")
# The solution retains 63% of variance
# Communalities are good
factor.fit(cor.books, pca.books$loadings) # fit: 0.71
pca.books = fa.sort(pca.books)

# book.a1 (biographies and historic books) has no clear factor assignment - repeat the analysis without it
D = D[, -which(names(D)=="book.a1")]
fa.parallel(D)
pca.books = principal(D, nfactors=4, rotate="quartimin")
# Retains 66% of variance and the communalities are high
pca.books = fa.sort(pca.books)

### Interpretation ###
# dim1 (22%) : book.a4, book.a7, book.a5 (literary canon, contemporary literature, poetry) : this factor clearly corresponds to a highbrow literary taste

# dim2 (17%) : book.a3, book.a6, book.a8 (youth literature, guidebooks, crime novels) : this is clearly a lighter more lowbrow literary taste.

# dim3 (15%) : book.a6, book.a9, book.a10 (guidebooks, popular science, travel) : this is clearly a factor corresponding to an information and non-fiction focused taste for books.

# dim4 (12%) : book.a2 (fantasy / sci-fi) : interpetation is obvious - this is a taste for fantasy / sci-fi alternative / epic / fictional narratives.

# Saving scales' scores
D.back$bookshigh = pca.books$scores[,1] # dim1
D.back$bookslow = pca.books$scores[,2] # dim2
D.back$booksknow = pca.books$scores[,3] # dim3
D.back$booksfant = pca.books$scores[,4] # dim4

# Correaltions with other scales
lowerCor(D.back[,190:201])


# Taste for press
D = D.back[, which(names(D.back)=="press.a1"):which(names(D.back)=="press.b10")]
# Press types
D.a = D[, 1:which(names(D)=="press.a7")]
# Distributions
histogram(expr(D.a), data=D.a, xlab="", layout=c(4,2))
pairs.panels(D.a)
cor.press.a = cor(D.a) # correlation matrix
fa.parallel(D.a) # parallel monte carlo analysis suggests 1 components
pca.press.a = principal(D.a, nfactors=1, rotate="quartimin")
# One dimension retain 38% of variance
factor.fit(cor.press.a, pca.press.a$loadings) # fit: 0.85 or 0.66 - poor?
# Moreover some communalities are really low and the parallel test almost indicated two components
# So repeat the analysis with 2 components
pca.press.a = principal(D.a, nfactors=2, rotate="quartimin")
# Two dimensions retain 54% of variance and the communalities are good
factor.fit(cor.press.a, pca.press.a$loadings) # fit: 0.78 or 0.80 - rather good
# Moreover now communalities are good, so this solution is preferable
pca.press.a = fa.sort(pca.press.a)

### Interpretation ###
# dim1 (38%) : press.a6, press.a5, press.a4, press.a3, press.a2, press.a7 (daily press, local press, opinion, culture / lifestyle, hobby / industry, tabloids) : this factor seems to corresponds mainly to a sort of general non-specific taste for press; a kind of press omnivore. So in general it may be considered general press-reading.

# dim2 (16%) : press.a2, press.a1, -press.a7 (hobby / industry, popular science, -tabloids) : this is cleary an information / knowledge focused taste which dislikes tabloids very much.

# Saving scales' scores
D.back$pressgen = pca.press.a$scores[,1]
D.back$pressknow = pca.press.a$scores[,2]

# distributions of the scales computed so far
histogram(expr(D.back[,190:203]), data=D.back, xlab="")
# nicely symmetric distributions :)
# Correlations between the scales
lowerCor(D.back[,190:203])

# Preferences regarding polish press
D.b = D[, which(names(D)=="press.b1"):which(names(D)=="press.b10")]
# Distributions
histogram(expr(D.b), data=D.b, xlab="", layout=c(5,2))
pairs.panels(D.b)
cor.press.b = cor(D.b) # correlation matrix
fa.parallel(D.b) # parallel monte carlo analysis suggests 2 components, but 3D solutions seems also good; I choose 3D
pca.press.b = principal(D.b, nfactors=3, rotate="quartimin")
# Three dimensions retain 70% of variance
factor.fit(cor.press.b, pca.press.b$loadings) # fit: 0.94 or 0.85 - good
pca.press.b = fa.sort(pca.press.b)



### Interpretation ###
# dim1 - press.b7, press.b5, press.b4, press.b2 (Uważam Rze, Do Rzeczy, Gazeta Polsa, Rzeczpospolita)
# This factor clearly correspond to a more conservative taste, since it consists exclusively of newspapers that are typically considered conservative.

# dim2 - press.b6, press.b9, press.b1, press.b8 (Gazeta Wyborcza, Polityka, Newsweek, Wprost)
# This dimension is stirctly related to a liberal, pro-european mainstream taste and, since it consists exclusively of the newspapers of such profile.

# Saving scales' scores
D.back$presscons = pca.press.b$scores[,1]
D.back$presslib = pca.press.b$scores[,2]
D.back$pressspec = pca.press.b$scores[,3]

# Distributions of the scales computed so far
histogram(expr(D.back[,190:205]), data=D.back, xlab="")
# nicely symmetric distribtions
# Correlations betwen the scales
lowerCor(D.back[,190:205])


# Taste for music
D = D.back[, which(names(D.back)=="music.a1"):which(names(D.back)=="music.a14")]
# Distributions
histogram(expr(D), data=D, xlab="", layout=c(5,3))
pairs.panels(D)
cor.music = cor(D) # correlation matrix
fa.parallel(D) # parallel monte carlo analysis suggests 4 components
pca.music = principal(D, nfactors=4, rotate="quartimin")
# Four dimensions retain 58% of variance
factor.fit(cor.music, pca.music$loadings) # fit: 0.85 - good
# There is one item - music.a11 (pop) - that has very low communality, so it shoudl be discarded
# Repeat the analysis without music.a11
D = D[, -which(names(D)=="music.a11")]
# Distributions
histogram(expr(D), data=D, xlab="", layout=c(5,3))
pairs.panels(D)
cor.music = cor(D) # correlation matrix
fa.parallel(D) # parallel monte carlo analysis suggests 4 components
pca.music = principal(D, nfactors=4, rotate="quartimin")
# Four dimensions retain 61% of variance
factor.fit(cor.music, pca.music$loadings) # fit: 0.87 - good
# communalities are also good
pca.music = fa.sort(pca.music)

### Interpretation ###
# dim1 - music.a10, music.a3, music.a13, music.a8, music.a1, music.a12 (jazz, blues, classical music, contemporary classical music, indie, ethnic / folk music)
# This factor seems to correspond to a kind of modern and calm highbrow taste that prefers rather sophisticated / more demanding genres and in general more creative music.

# dim2 - music.a7, music.a5, music.a4, music.a9 (techno, electronic music, house, hip-hop)
# This dimension corresponds to a taste for modern electronic and club music.

# dim3 - music.a2, music.a14 (heavy metal, rock)
# This factor is clearly about rock and heavy metal taste

# dim4 - music.a3, music.a9, music.a6 (blues, hip-hop, reggea)
# This dimension clearly corresponds to taste for black / afro-american music

# Saving scales' scores
D.back$musichigh = pca.music$scores[,1]
D.back$musicmodern = pca.music$scores[,2]
D.back$musicrock = pca.music$scores[,3]
D.back$musicafro = pca.music$scores[,4]

# Distributions of the scales
histogram(expr(D.back[,190:209]), data=D.back, xlab="")
# all are nicely symmetric
# Correlations between the scales
lowerCor(D.back[,190:209])
# It should be noted in the end that nice and meaningful correlation structures indicate that the chosen method of missing data imputation was efficient and non-biased. Vivat MICE algorithm!

# Saving the full dataser
write.csv(D.back, file="MainData/MainData5.csv", row.names=FALSE)
D = D.back
save(D, file="MainData/MainData5.RData")

# Make a new dataset without item banks
# get rid of the tv item banks
D = D[,-(which(names(D)=="tv.a1"):which(names(D)=="tv.b9"))]
# get rid of the movies item bank
D = D[,-(which(names(D)=="movie.a1"):which(names(D)=="movie.a10"))]
# get rid of the books item bank
D = D[,-(which(names(D)=="book.a1"):which(names(D)=="book.a10"))]
# get rid of the press item banks
D = D[,-(which(names(D)=="press.a1"):which(names(D)=="press.b10"))]
# get rid of the music item bank
D = D[,-(which(names(D)=="music.a1"):which(names(D)=="music.a14"))]
# get rid of the civic activity item bank
D = D[,-(which(names(D)=="socact1"):which(names(D)=="socact14"))]
# get rid of the social capital item banks
D = D[,-(which(names(D)=="scknow1"):which(names(D)=="schelp15"))]
# get rid of the place attachment item banks
D = D[,-(which(names(D)=="attgen1"):which(names(D)=="attmult18"))]

# Save a new reduced dataset
write.csv(D, file="MainData/MainData6.csv", row.names=FALSE)
save(D, file="MainData/MainData6.RData")

rm(list=ls())

# This is it folks!