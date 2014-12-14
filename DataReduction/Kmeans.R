# This script perforsm k-means clustering on scaled variables and dimensions after the PCA and MCA
library(NbClust)
library(lattice)
library(doBy)
library(smacof)
library(psych)
library(mvnormtest)
library(MASS)
library(candisc)
library(biotools)
library(vcd)
source("DataReduction/KmeansHelper.R")
source("HelperFunctionsMisc/ComputingMisc.R")

load("MainData/MainData8.RData") # loads the main dataset as an object called 'D'

D.back = D # full backup dataset

vars = names(D.back)[c(11:12, 29:30, 37, 44:dim(D)[2])] # variables to be selected for the clustering

D = D.back[, vars]
D.s = scale(D) # scaled data matrix

wssplot(D.s, nc=15) # scree plot for within groups sum of squares

set.seed(5050) # set the pseudorandom numbers generator for the NbClust
nc <- NbClust(D.s, min.nc=2, max.nc=15, method="kmeans", index="all") 
# optimal number of clusters is 2 but the second best is 3.
# Thus I choose 3, since 2 is too crude a classification
# Optimal cluster numbers were determined by a majority voting
# There were 6 votes for 5 clusters and 8 for 2; other options were negligible

# Further analysis of optimal clustering
table(nc$Best.n[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]), xlab="Number of a cluster", ylab="Number of criteria", main="Number of clusters chosen by 26 criteria", col="gray15")

# Kmeans clustering
set.seed(1234) # set the pseudorandom numbers generator
Kpart = kmeans(D.s, centers=3, iter.max=40, nstart=50)
# I choose a lot of random initial conditions to guarantee that the best partition will be obtained
# add cluster assignments to the dataset
D.back$cluster = Kpart$cluster
D.s = as.data.frame(D.s)
D.s$cluster = Kpart$cluster
centroids = Kpart$centers # cluster profiles
centroids.dist = dist(centroids) # distances between clusters
# Plot the profiles
par(mfrow=c(1,1))
plot(centroids[1,], type="l", ylab="dimension value", xlab="", xaxt="n", lwd=2, ylim=c(-1.5,1.5))
axis(1, at=1:29, labels=colnames(centroids), las=3, cex.axis=.7)
lines(centroids[2,], col="red", lwd=2)
lines(centroids[3,], col="blue", lwd=2)
for(i in 1:29) {abline(v=i, lty=2, lwd=.7)}
legend("topright", col=c("black", "red", "blue"), legend=1:3, lty=1, cex=.9)

# Now the same, but one in one frame
par(mfrow=c(2,2))
# all cluster profiles
plot(centroids[1,], type="l", ylab="dimension value", xlab="", xaxt="n", lwd=2, ylim=c(-1.5,1.5))
axis(1, at=1:29, labels=colnames(centroids), las=3, cex.axis=.8)
lines(centroids[2,], col="red", lwd=2)
lines(centroids[3,], col="blue", lwd=2)
# cluster 1
plot(centroids[1,], type="l", ylab="dimension value", xlab="", col="black", xaxt="n", lwd=2, ylim=c(-1.5,1.5), main="Cluster 1")
axis(1, at=1:29, labels=colnames(centroids), las=3, cex.axis=.8)
# cluster 2
plot(centroids[2,], type="l", ylab="dimension value", xlab="", col="red", xaxt="n", lwd=2, ylim=c(-1.5,1.5), main="Cluster 2")
axis(1, at=1:29, labels=colnames(centroids), las=3, cex.axis=.8)
# cluster 3
plot(centroids[3,], type="l", ylab="dimension value", xlab="", col="blue", xaxt="n", lwd=2, ylim=c(-1.5,1.5), main="Cluster 3")
axis(1, at=1:29, labels=colnames(centroids), las=3, cex.axis=.8)
# end of plotting
par(mfrow=c(1,1)) # back to standard single frame

# ANOVA tests
# initialize data frame for storing test values
Ftest.df = data.frame(Fval=vector(mode="numeric", length=29), pval=vector(mode="numeric", length=29))
rownames(Ftest.df) = colnames(centroids)
for(i in 1:29) {
      model = aov(D.s[,i] ~ cluster, data=D.s)
      Ftest.df[i, 1] = as.numeric(unlist(summary(model))[7])
      Ftest.df[i, 2] = as.numeric(unlist(summary(model))[9])
}
# In order to get only more significant differences and not capitalize on chance I take alpha level 0f 0.01
Ftest.df.sig = Ftest.df[Ftest.df$pval < 0.01, ] # only significant differences

# sport int : 1 > 3 
pairwise.t.test(D.s$sportint, D.s$cluster, p.adjust.method = "bonferroni")
# Footbal : 1, 2 > 3
pairwise.t.test(D.s$football, D.s$cluster, p.adjust.method = "bonferroni")
# Civic : 1 > 2, 3
pairwise.t.test(D.s$civic, D.s$cluster, p.adjust.method = "bonferroni")
# TV low brow : 1, 2 > 3
pairwise.t.test(D.s$tvlowbrow, D.s$cluster, p.adjust.method = "bonferroni")
# TV high brow : 1, 2 > 3
pairwise.t.test(D.s$tvhighbrow, D.s$cluster, p.adjust.method = "bonferroni")
# TV pop : 1 > 2 > 3 
pairwise.t.test(D.s$tvpop, D.s$cluster, p.adjust.method = "bonferroni")
# TV cons : 1, 2 > 3
pairwise.t.test(D.s$tvcons, D.s$cluster, p.adjust.method = "bonferroni")
# Movies epic : 1 > 2, 3
pairwise.t.test(D.s$moviesepic, D.s$cluster, p.adjust.method = "bonferroni")
# Movies high : 2 > 1 > 3
pairwise.t.test(D.s$movieshigh, D.s$cluster, p.adjust.method = "bonferroni")
# Books high : 1 > 2, 3
pairwise.t.test(D.s$bookshigh, D.s$cluster, p.adjust.method = "bonferroni")
# Books low : 2 > 1 > 3 
pairwise.t.test(D.s$bookslow, D.s$cluster, p.adjust.method = "bonferroni")
# Books know : 1 > 2, 3
pairwise.t.test(D.s$booksknow, D.s$cluster, p.adjust.method = "bonferroni")
# Press gen : 1, 2 > 3
pairwise.t.test(D.s$pressgen, D.s$cluster, p.adjust.method = "bonferroni")
# Press cons : 1, 2 > 3
pairwise.t.test(D.s$presscons, D.s$cluster, p.adjust.method = "bonferroni")
# Press lib : 1, 2 > 3
pairwise.t.test(D.s$presslib, D.s$cluster, p.adjust.method = "bonferroni")
# Press spec : 1, 2 > 3 
pairwise.t.test(D.s$pressspec, D.s$cluster, p.adjust.method = "bonferroni")
# Music high : 1 > 3 > 2
pairwise.t.test(D.s$musichigh, D.s$cluster, p.adjust.method = "bonferroni")
# Music afro : 1, 2 > 3
pairwise.t.test(D.s$musicafro, D.s$cluster, p.adjust.method = "bonferroni")
# Cult cap : 1 > 2, 3
pairwise.t.test(D.s$cultcap, D.s$cluster, p.adjust.method = "bonferroni")
# Wealth : 2 > 1
pairwise.t.test(D.s$wealth, D.s$cluster, p.adjust.method = "bonferroni")

# Visual inspection of this results using a data frame
c1 = c(1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,-1)
c2 = c(0,1,-1,1,1,0,1,-1,1,-1,1,-1,1,1,1,1,-1,1,-1,1)
c3 = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,0)
Ftest.df.sig = cbind(Ftest.df.sig, c1,c2,c3)

# Now it is feasible to come up with some interpretation

### Interpretation ###
# additional analyses:
ISCOtab = table(D.back$cluster, D.back$ISCObroad)
assocstats(ISCOtab) # significant association; Cramers-V = 0.217
eduprogtab = table(D.back$cluster, D.back$eduprog)
assocstats(eduprogtab) #  significant association; Crammers-V = 0.232
worktimetab = table(D.back$cluster, D.back$worktime)
assocstats(worktimetab) # significant association: Crammers-V = 0.237
homestattab = table(D.back$cluster, D.back$homestatus)
assocstats(homestattab) # insignificant association; Crammers-V = 0.128
cartab = table(D.back$cluster, D.back$car)
assocstats(cartab) # insignificant assocation; Crammers-V = 0.50

### INTERPRETATION ###
# Cluster 1 - Urban professionals (n = 100)
# High profile of cultural consumption and capital. Mostly professionals (with a high fraction of flexible worktime). To some degree dominated by peope of humanities/social sciences background

# Cluster 2 - students (n = 74)
# Mostly not working studetns with some prevalence of law/management students. They have mixed profile of cultural consumption and not too high cultural capital

# Cluster 3 - culturally disinterested (n = 57)
# This cluster groups both workking and non-working people. The trait they share is a low cultural profile - a general lack of interest in (at least typical) cultural consumption.

cluster = D.s$cluster
cluster[cluster==1] = "Wolne_Zawody"
cluster[cluster==2] = "Studenci"
cluster[cluster==3] = "Kulturalnie_wycofani"
cluster = as.factor(cluster)
cluster = factor(cluster, levels(cluster)[c(3,2,1)])
D.s$cluster = cluster
D.back$cluster = cluster

# LDA - sepearatin the clusters
# distributions of the variables in D.s
histogram(expr(D.s[,1:29]), data=D.s, xlab="") # all are very nicely symmetric
# test of multivariate normality
mshapiro.test(as.matrix(t(D.s[,1:29]))) # the data is not perfeclty multivariably normal
# Box M test of covariance matrices homogeneity
boxM(D.s[,1:29], D.s$cluster) # not homogeneous
# This is normal for not data that is high dimensional and does not have a perfect multivariate normal distribution. Check visually mean squared error of differences between covariances matrices
cov1 = cov(D.s[D.s$cluster=="Profesjonaliści", 1:29])
cov2 = cov(D.s[D.s$cluster=="Studenci", 1:29])
cov3 = cov(D.s[D.s$cluster=="Kulturalnie_wycofani", 1:29])
dcov1cov2 = (cov1-cov2)^2
dcov1cov3 = (cov1-cov3)^2
dcov2cov3 = (cov2-cov3)^2
dcov = (dcov1cov2+dcov1cov3+dcov2cov3) / 3
cov = (cov1+cov2+cov3) / 3 # they are clearly not homogeneous
# But Manova and LDA will be performed anyway

# MANOVA - as a basis for LDA
manova = manova(as.matrix(D.s[,1:29]) ~ D.s$cluster)
# LDA
lda1 = candisc(manova) # All 2 canonical functions are significant
cv = lda(cluster ~ ., data=D.s, prior=rep(1/3, 3), CV=TRUE)$class # cross-validation predictions
mean(cv == D.s$cluster) # 86% of accuracy on leave-one-out cross-validation
table(D.s$cluster, cv)
lda2 = lda(cluster ~., data=D.s, prior=rep(1/3, 3))
pred = predict(lda2, D.s)

# Jus in case : save cannonical scores
D.back$can1 = lda1$scores[,2]
D.back$can2 = lda1$scores[,3]

# Save the workspace
save.image("DataReduction/clustWS.RData")
# Save the dataset
D = D.back
save(D, file="MainData/MainData9.RData")

# The end
rm(list=ls())