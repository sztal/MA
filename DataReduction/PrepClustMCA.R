# This is a preparatory script before the MCA and the cluster analysis
# It is goal is to get rid of the variables that will be of no use due to extreme distributions or lack of variability
# Moreover it will reorder (and sometimes recombine) factor levels of the variables in order to facilitate intepretation of the Multiple Correspondnce Analysis
library(lattice)
library(psych)
library(mice)
source("Imputation/ImputeHelper.R")

load("MainData/MainData6.RData")
table(D$marital) # there are only 10 persons who are married
# So I collapse formal and non-formal permanent relationships together
levels(D$marital)[2:3] = "relationship"

# Histogram of timeWwa
D$timeWwa = as.numeric(D$timeWwa)
D$age = as.numeric(D$age)
histogram(~ D$timeWwa, D, xlab="")
# There is cleatly a structure to this double-peaked distribution. There are actually two subgroups in a sample - people that were born in Warsaw and those who moved in at some point. Therefore there is one group with approximately normal distribution - these are the people born in Warsaw - which results from the fact that the their timeWwa distribution follows their age distribution. The second group - the incomers - have right-skewed distribution because most of them came relatively recently (usually to study), but some are at place for a longer period of time now.
# Check the histogram to see that
histogram(~ timeWwa + age, D, xlab="") # see: the age distribution is approximately normal
# see the distributions only for people born in Warsaw
histogram(~ timeWwa + age, D[D$age == D$timeWwa, ], xlab="") # the distributions are identical
# see the distributions only for people who came to Warsaw
histogram(~ timeWwa + age, D[D$age != D$timeWwa, ], xlab="")
# the age distribution is approximately normal, but the timeWwa distributions is significantly right-skewed and this proves the initial hypothesis concerning the structure of the total distribution

table(D$dog) # this variable is fine
table(D$edu)
table(D$eduprog)
table(D$edutime)
table(D$worktime) # this is problematic; it will be recoded into job (part / full time) / no_job / flexible
levels(D$worktime)[c(2,4)] = c("job", "job")
D$worktime = factor(D$worktime, levels(D$worktime)[c(3,2,1)])
table(D$fatheredu) # the don't_know category is to small - recode it to NA (later on impute)
D[D$fatheredu=="don't_know", "fatheredu"] = NA
D$fatheredu = droplevels(D$fatheredu) # droplevels
table(D$motheredu) # again - don't_know to NA
D[D$motheredu=="don't_know", "motheredu"] =NA
D$motheredu = droplevels(D$motheredu)
table(D$grandedu) # this one is good, but need reordering

D$income = as.numeric(D$income)
D$lifestd = as.numeric(D$lifestd)
histogram(~ income + lifestd, D, xlab="") # these are good, but check the correlation
lowerCor(D[,which(names(D)=="income"):which(names(D)=="lifestd")])
# this variables will be normalized and used together with the scales of cultural preferences and the dimensions of MCA
table(D$cars) # it is ok, but need reordering
table(D$hometype) # it is ok
table(D$homestatus) # other is to small; since most of the others were students halls or social flats I combine it with the rent category (this may be easily checked using the "Dane/raw244.csv" dataset)
levels(D$homestatus)[1] = "rent"
D$homestatus = factor(D$homestatus, levels(D$homestatus)[c(2,1,3)])
table(D$tv) # it is ok
table(D$tablet) # it is ok
table(D$internetuse) # small variation; it rather will not be used
# now check all the overdispersed variables: tvtime, movie30 and book12
histogram(~ tvtime + book12 + movie30, D, xlab="") # all are really overdispersed
# let's check if they can be reduced somehow; otherwise - discard
lowerCor(D[, c("tvtime", "book12", "movie30")]) # nope
# I choose to discard them since they are really skewed and overdispersed and thus they will distort solutions of clustering procedure. Moreover, there are already multip variables on tv, books and movies etc.
D = D[, -c(which(names(D)=="tvtime"), which(names(D)=="book12"), which(names(D)=="movie30"))]

table(D$moviefreq)
table(D$bookfreq) # it is ok, but factor levels need to be reordered
D$bookfreq = factor(D$bookfreq, levels(D$bookfreq)[c(3,2,1)])
table(D$pressfreq) # it is ok, but needs reordering
D$pressfreq = factor(D$pressfreq, levels(D$pressfreq)[c(3,2,1)])
table(D$musicfreq) # it is ok, but need reordering
D$musicfreq = factor(D$musicfreq, levels(D$musicfreq)[c(2,1)])
table(D$theater) # it is ok
table(D$opera) # it is ok
table(D$cinema) # rarely is to rare (sic!) - so recode
levels(D$cinema)[1] = "sometimes" # now it is ok
table(D$art)
table(D$livemusic)
table(D$club)
table(D$sportshow)

D$sportint = as.numeric(D$sportint)
D$football = as.numeric(D$football)
histogram(~ sportint + football, D, xlab="") # ok
table(D$trainfreq) # neves category is small, but I can not combine it with anything
table(D$trainfav) # simillar problem
table(D$cdquant)
table(D$bookquant)
table(D$artquant)
histogram(~ age, D, xlab="") # it is ok
table(D$ISCObroad)
table(D$incomeclass) # it is ok

### Now impute the missing datapoints that have bee introduced due to recoding ###
NAs = apply(D, 2, numNA)
# helper vector with the types of the variables
vartypes = vector(mode="character", length=dim(D)[2])
for(i in 1:dim(D)[2]) {vartypes[i] = class(D[,i])}
# helper vector with levels' numbers
numlev = vector(mode="numeric", length=dim(D)[2])
for(i in 1:dim(D)[2]) {numlev[i] = length(levels(D[,i]))}
vars = data.frame(type = vartypes, levels = numlev)
# prepare the method vector for mice algorithm by hand (exclude id)
methods = vector(mod="character", length=dim(D)[2])
for(i in 1:dim(D)[2]) {
      if(vars[i,1] == "numeric" | vars[i,1] == "integer") methods[i]="pmm"
      else {
            if(vars[i,2] == 2) methods[i] = "logreg"
            else methods[i] = "lda"
      }
}
# check if methods assignment was correct
vars = cbind(vars, methods)
# MICE imputation (the pseudrandom number generator is set)
D.imp = D[,3:dim(D)[2]]
methods = methods[3:length(methods)]
Imp = mice(D.imp, m=20, seed=1007, method=methods)
# There is some error I can not figure out

# for some reason ISCObroad makes MICE crash, so I impute without it
Di = D[,c(2:38, 40:dim(D)[2])]
methods.i = methods[c(2:38,40:dim(D)[2])]
methods.i[65:66] = "pmm"
Imp = mice(Di, m=20, seed=105, method=methods.i)
ImpL = actualImp(Imp$imp)
ImpErr = ImpOut(ImpL, Di)[[1]] # Uncertainity of imputed values
ImpVal = ImpOut(ImpL, Di)[[2]] # Dominant imputed values
# motheredu imputation is quite bad, but this is just one observation so it is okay
Di.2 = mapImpToData(ImpVal, Di) # good
Di = Di.2
D.back = D # backup dataset
D[, c(2:38,40:dim(D)[2])] = Di # replace the missing values
D.back == D # fine!

# So now the dataset is prepared for the Multiple Correspondence Analysis
save(D, file="MainData/MainData7.RData")
########################################################################################## IMPORTANT NOTE: .csv data files do not preserve informtion about factor levels orderings, so it is recommended to use .RData files from now on
##########################################################################################

rm(list=ls())
# This is it folks!