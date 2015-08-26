############################
### EXPLORATORY ANALYSIS ###
############################

### This script performs exploratory analysis
### of the main dataset, the places dataset and the people-places network dataset
###
### Its aim is to provide some preliminary insight into the nature of the dataset
### that will hopefuly make the analysis concerning the main hypotheses
### more informative and graspable.

### Load packages
library(igraph)
library(lattice)
library(latticeExtra)
library(poweRlaw)
library(dplyr)
library(xtable)
source("HelperFunctionsMisc/ComputingMisc.R")
source("Networks/NetworkMethods.R")

###########################################
### Exploratory analysis of places data ###
###########################################
### First I check the distribution of the places popularity (number of indications)
### in order to look for a power-law structure.
### Moreover I will present the most popular places
### and create a map of places in regard to cluster profiles of their visitors.

### Load places data
load("Places/PlaceData.RData")

### load main (respondents) data set
load("MainData/MainData11.RData")

### Set trellis engine to black-and-white mode
trellis.device(color=FALSE, new=FALSE)

### Due to a mistake one node has to be deleted in this analysis,
### because it is duplicated
Pdat.backup <- Pdat
Pdat <- Pdat[-which(rownames(Pdat) == "ogród_saski"), ]

# Frequency distribution of popularity
pop_dist = prop.table(table(Pdat$popularity))
# Denstiy plot for popularity
densityplot(~ popularity, data=Pdat,
            plot.points="jitter", auto.key=TRUE,
            xlab="Popularność", ylab="Gęstość prawdopodobieństwa") # very power-lawish...
### Formal check of the power-lawness
fit = power.law.fit(Pdat$popularity, xmin=1, implementation="plfit")
fit$KS.p # Kolmogorov-Smirnov test proves that the distribution is indeed power-law
summary(Pdat$popularity)

### Most popular places ###
### First I store names of places as a variables to facilitate use of the dplyr package
name = rownames(Pdat)
Pdat$name <- name

### places with more than 5 visitors (others will be neglected)
filter(Pdat, popularity >= 5)[, c("popularity", "name")] %>% arrange(desc(popularity)) %>% dim ### there are 83 places indicated by 5 or more persons (out of 567)

### In general there are almost only:
###   - clubs
###   - cafes
###   - parks and waterfronts
###   - nice recreational/representative areas (like Vistula Boulevard and Krakowskie Przedmieście)
###   - some cinemas
###   - big shopping malls

### 10 most popular places
arrange(Pdat, desc(popularity))[c("popularity", "name")] %>% head(n=10)
# popularity                         name
# 1          71      filtry/pole_mokotowskie
# 2          41                     pawilony
# 3          36 park_łazienkowski/ujazdowski
# 4          34                   buw/ogrody
# 5          33            stare/nowe_miasto
# 6          31                      arkadia
# 7          29                 złote_tarasy
# 8          27       schody/plaża_nad_wisłą
# 9          26  krk_przedmieście/nowy_świat
# 10         26                    nad_wisłą

### Clearly this groups is dominated by bigger locations / areas. These are mostly:
###   - parks and waterfronts
###   - representational areas
###   - shopping malls
###
### The only two exceptions seem to be Warsaw University Library (BUW) and Pawilony (popular complex of relatively cheap pubs and cafes)

### Now we will look how social heterogeneity of places 
### (entropy of visitors' clusters distribution) is related to popularity
### Places with 5 or more visitors
xyplot(ent ~ popularity, data=filter(Pdat, popularity >= 5),
          jitter.data=TRUE, alpha=.7, pch=1,
          panel = function(x, y) {
            panel.stripplot(x, y)
            panel.loess(x, y, col="red", lty=2)
            panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(ent)),
                         col = "blue")
            })

### Names of places
n10 <- c("Grawitacja i Jaś i Małgosia", "Pawilony", "Plan B", "Pole Mokotowskie",
         "Kino Muranów", "BUW", "CH Arkadia", "Stare Miasto", "Bulwary Wiślane",
         "Kinoteka", "Las Kabacki", "CH Złote Tarasy", "CH Galeria Mokotów",
         "Plaża nad Wisłą", "Krakowskie Przedmieście i Nowy Świat", "Łazienki",
         "Nad Wisłą", "Kafefajka", "Kępa Potocka", "Klub 55", "Cafe Kulturalna",
         "Park Skaryszewski", "Znajomi Znajomych", "Temat Rzeka", "Państwomiasto",
         "Park Szczęśliwicki", "Klub Hybrydy", "Ogród Saski", "Klub Park",
         "Hula Kula")
n15 <- c("Pawilony", "Plan B", "BUW", "CH Arkadia", "Stare Miasto",
         "Bulwary Wiślane", "CH Złote Tarasy", "Plaża nad Wisłą",
         "Krakowskie Przedmieście i Nowy Świat", "Łazienki", "Nad Wisłą",
         "Park Skaryszewski", "Temat Rzeka")

### Places with 10 or more visitors
pos <- rep(4, 30)
pos[4] <- 2
pos[12] <- 1
pos[13] <- 3
pos[28] <- 3
pos[9] <- 1
pos[c(14, 17, 3)] <- 3
pos[c(16)] <- 1
pos[c(20,18,29)] <- 2
pos[6] <- 1
xyplot(ent ~ popularity, data=filter(Pdat, popularity >= 5),
        jitter.data=TRUE, alpha=.7, pch=1, col="blue",
        xlab="Popularność", ylab="Entropia",
          panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                panel.loess(x, y, col="red", lty=2)
                panel.abline(h = entropy(D$cluster), lty=2, lwd=2,
                             alpha=.5, col="blue")
                panel.text(x = filter(Pdat, popularity >= 10)$popularity,
                           y = filter(Pdat, popularity >= 10)$ent,
                           labels = n10,
                           pos=pos, cex=.8, alpha=.9)
          })
### Places with 15 or more visitors
xyplot(ent ~ popularity, data=filter(Pdat, popularity >= 15),
          jitter.data=TRUE, alpha=.7, pch=0,
          panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                panel.loess(x, y, col="red", lty=2)
                panel.abline(h = filter(Pdat, popularity >= 15) %>% summarise(mean(ent)),
                             col = "blue")
                panel.text(x = filter(Pdat, popularity >= 15)$popularity,
                           y = filter(Pdat, popularity >= 15)$ent,
                           labels = filter(Pdat, popularity >= 15)$name,
                           pos=1, cex=.8)
          })

### Places by entropy (10 or more visitors)
filter(Pdat, popularity >= 10)[, c("ent", "name", "popularity")] %>% arrange(desc(ent))

### Entropy of total cluster distribution in the respondent dataset (main dataset)
entropy(D$cluster)
### Average entropy of places:
### 5 or more visitors (82 places)
filter(Pdat, popularity >= 5) %>% summarise(mean(ent)) - entropy(D$cluster)
t.test(filter(Pdat, popularity >= 5)[, "ent"],
       mu = entropy(D$cluster), alternative="less")
### 10 or more visitors (30 places)
filter(Pdat, popularity >= 10) %>% summarise(mean(ent)) - entropy(D$cluster)
t.test(filter(Pdat, popularity >= 10)[, "ent"],
       mu = entropy(D$cluster), alternative="less")
### 15 or more visitors (15 places)
filter(Pdat, popularity >= 15) %>% summarise(mean(ent)) - entropy(D$cluster)
t.test(filter(Pdat, popularity >= 15)[, "ent"],
       mu = entropy(D$cluster), alternative="less")
### 20 or more visitors (12 places)
filter(Pdat, popularity >= 20) %>% summarise(mean(ent)) - entropy(D$cluster)
t.test(filter(Pdat, popularity >= 20)[, "ent"],
       mu = entropy(D$cluster), alternative="less")
### 30 or more visitors (6 places)
filter(Pdat, popularity >= 30) %>% summarise(mean(ent)) - entropy(D$cluster)
t.test(filter(Pdat, popularity >= 30)[, "ent"],
       mu = entropy(D$cluster), alternative="less")
### 40 or more visitors (2 places)
filter(Pdat, popularity >= 40) %>% summarise(mean(ent)) - entropy(D$cluster)
t.test(filter(Pdat, popularity >= 40)[, "ent"],
       mu = entropy(D$cluster), alternative="less")

### Places in cultural space 
###(2 dimensions of the solution of the discriminant analysis on clustering space)
xyplot(can1 ~ can2, data=filter(Pdat, popularity >= 5),
       jitter.data=FALSE, alpha=.7, cex=.4, pch=0, ylim=c(-1,1), xlim=c(-1.5,1.5),
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.text(x = filter(Pdat, popularity >= 10)$can2,
                        y = filter(Pdat, popularity >= 10)$can1,
                        labels = filter(Pdat, popularity >= 10)$name,
                        pos=1, cex=.8)
       })

### Now we look at the relationshib between social capital and entropy in places
### SOCCONT
### 5+ visitors
xyplot(soccont ~ ent, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(soccont)),
                          col = "blue")
       })
### RESMOB
### 5+ visitors
xyplot(resmob ~ ent, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(resmob)), col = "blue")
       })
### Clearly there is no pattern
###   - more diverse places do not attract more 'socially embedded' people



### Now we look at dominant clusters in places and their
### relations to social capital, popularity and entropy

### Dominant clusters in popularity classes
### 5+ visitors
table(filter(Pdat, popularity >= 5)$dcluster)
### 10+ visitors
table(filter(Pdat, popularity >= 10)$dcluster)
### 15+ visitors
table(filter(Pdat, popularity >= 15)$dcluster)

### Dominant cluster and popularity
summarise(group_by(
      filter(Pdat, popularity >= 5), dcluster), mean(popularity))
summary(lm(popularity ~ dcluster, data = filter(Pdat, popularity >= 5)))
### similar means with not significant differences

### Dominant cluster and entropy
summarise(group_by(
      filter(Pdat, popularity >= 5), dcluster), mean(ent))
summary(lm(ent ~ dcluster, data = filter(Pdat, popularity >= 5)))
### similar means with not significant differences

### Dominant cluster and soccont
summarise(group_by(
      filter(Pdat, popularity >= 5), dcluster), mean(soccont))
summary(lm(soccont ~ dcluster, data = filter(Pdat, popularity >= 5)))

### Dominant cluster and resmob
summarise(group_by(
      filter(Pdat, popularity >= 5), dcluster), mean(resmob))
summary(lm(resmob ~ dcluster, data = filter(Pdat, popularity >= 5)))


### Now we look and correlation between the cluster proportions and popularity as well as social capital

### Popularity
### cluster I
xyplot(popularity ~ cluster1, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(popularity)),
                          col = "blue")
       })
### no association

### cluster II
xyplot(popularity ~ cluster2, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(popularity)),
                          col = "blue")
       })
### no association

### cluster III
xyplot(popularity ~ cluster3, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(popularity)),
                          col = "blue")
       })
### no association

### Social capital
### RESMOB
### cluster I
xyplot(resmob ~ cluster1, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(resmob)),
                          col = "blue")
       })
### no association

### cluster II
xyplot(resmob ~ cluster2, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(resmob)),
                          col = "blue")
       })
### no association

### cluster III
xyplot(resmob ~ cluster3, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(resmob)),
                          col = "blue")
       })
### also nothing really interesting...

### SOCCONT
### cluster I
xyplot(soccont ~ cluster1, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(soccont)),
                          col = "blue")
       })
### there seems to be some kind of maybe quadratic association

### cluster II
xyplot(soccont ~ cluster2, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(soccont)),
                          col = "blue")
       })
### no association

### cluster III
xyplot(soccont ~ cluster3, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=0,
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.loess(x, y, col="red", lty=2)
             panel.abline(h = filter(Pdat, popularity >= 5) %>% summarise(mean(soccont)), col = "blue")
       })
### also nothing really interesting...

### So in general there may be something going on in places with an optimal number of visitors from the cluster I.

### Lastly, I prepare a map of places in a space of their visitors in terms of their cluster assignments to see which places are visited by which types of people.
### Sine three-categorical distribution has only two degrees of freedom I can make such a map in only two dimensions without any loss of information. Thus, I will use only cluster I and cluster II proportions.
pos=rep(1,30)
pos[9] <- 2
pos[17] <- 4
pos[c(2,7,12)] <- 2
pos[24] <- 3
xyplot(cluster1 ~ cluster2, data=filter(Pdat, popularity >= 5),
       jitter.data=TRUE, alpha=.7, pch=1, col="blue",
       xlab="Studenci/Dzieci zamożnej klasy średniej",
       ylab="Wolne Zawody",
       panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             panel.text(x = filter(Pdat, popularity >= 10)$cluster2,
                        y = filter(Pdat, popularity >= 10)$cluster1,
                        labels = n10,
                        pos=pos, cex=.8)
             })