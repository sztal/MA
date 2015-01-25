# Descriptive statistics for the sample of individuals
library(xtable)
source("HelperFunctionsMisc/ComputingMisc.R")

# Load the data
load("./MainData/MainData13.RData")

# Education time and level - crosstabulation
edutab <- table(D$edutime, D$edu)

# Simple unidimensional descriptives for all variables
maritaltab <- table(D$marital)
edutab <- table(D$edu)
eduprog <- table(D$eduprog)
edutimetab <- table(D$edutime)
worktimetab <- table(D$worktime)
fatheredutab <- table(D$fatheredu)
motheredutab <- table(D$motheredu)
grandedutab <- table(D$grandedu)
incometab <- table(D$income)
carstab <- table(D$cars)
hometypetab <- table(D$hometype)
homestatustab <- table(D$homestatus)
tvtab <- table(D$tv)
tablettab <- table(D$tablet)
moviefreqtab <- table(D$moviefreq)
bookfreqtab <- table(D$bookfreq)
pressfreqtab <- table(D$pressfreq)
musicfreqtab <- table(D$musicfreq)
theatertab <- table(D$theater)
operatab <- table(D$opera)
cinematab <- table(D$cinema)
arttab <- table(D$art)
livemusictab <- table(D$livemusic)
clubtab <- table(D$club)
sportshowtab <- table(D$sportshow)
bookquanttab <- table(D$bookquant)
cquanttab <- table(D$cdquant)
artquanttab <- table(D$artquant)
ISCObroadtab <- table(D$ISCObroad)
incomeclasstab <- table(D$incomeclasstab)
clustertab <- table(D$cluster)
netcommtab <- table(D$netcomm)

# List of quantitative variables
quantvar = names(D)[c(12,29:30,34,37:67,69:78)]

# Function that generates a table of quantitative variables
quantab <- function(D, vars, stats=c("min","max","mean","sd","median")) {
      D = D[, vars]
      n = length(vars)
      m = length(stats)
      Tab = matrix(0, nrow=n, ncol=m)
      for(i in 1:m) {
            stat = stats[i]
            vals = round(apply(D, 2, stat, na.rm=T), 2)
            Tab[, i] = vals
      }
      colnames(Tab) = stats
      rownames(Tab) = vars
      return(Tab)
}

# Generating a table with descriptives for quantitative variables
# Getting some variables rescaled to range of 1 to 7
D2 = D[, quantvar]
for(i in 12:37) D2[, i] = rescale(D2[, i], min=1, max=7)
Tab = quantab(D2, quantvar)
ns = apply(D[, quantvar], 2, numNA, n=TRUE)
Tab = cbind(Tab, ns)

# Additionaly general attachment to Warsaw is rescaled to range 1 to 5
D$attgen = rescale(D$attgen, min=1, max=5)

# Create new dataset with rescaled variables
D[, quantvar] = D2
save(D, file="./MainData/MainData14.RData")
write.csv(D, file="./MainData/MainData14.csv", row.names=FALSE)
      