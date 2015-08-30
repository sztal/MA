# This is a file with helper funtions for network datasets derivation

getNames <- function(P) {
      # This function gets a list of all unique place names from a place dataset
      N = vector(mode="character", length=dim(P)[1]*dim(P)[2])
      for(i in 1:dim(P)[1]) {
            for(j in 1:dim(P)[2]) {
                  ind = (i-1)*dim(P)[2] + j
                  name = as.character(P[i, j])
                  N[ind] = name
            }
      }
      N = N[!(N == "" | is.na(N) | is.null(N))]
      N = tolower(N)
      N = unique(N)
      return(N)
}

# This function transform all columns in a place dataset to character vector and then to lower cases
toLower <- function(P) {
      for(i in 1:dim(P)[2]) {
            P[,i] = as.character(P[,i])
            P[,i] = tolower(P[,i])
      }
      return(P)
}

# This function get all palces from a place dataset as a vector - it can be used to compute distributions of place names
getNames <- function(P) {
      Names = c()
      for(i in 1:dim(P)[1]) {
            n = as.character(P[i, ])
            n = n[!(n=="" | is.na(n) | is.null(n))]
            Names = append(Names, n)
      }
      Names = sort(Names)
      return(Names)
}

# This function give a proposition of approximate mathing of names
approxMatch <- function(p1, N) {
      len = length(p1)
      match = vector(mode="character", length=len)
      for(i in 1:len) {
            if(p1[i] != "") {
                  prop = agrep(p1[i], N)[1]
                  match[i] = N[prop]
            }
            else match[i] = NA
      }
      DF = data.frame(p1, match=match)
      return(DF)
}

# Function changing factor columns to character columns
toCharacter <- function(P) {
      for(i in 1:dim(P)[2]) {
            if(class(P[,i]) == "factor") P[,i] = as.character(P[,i])
      }
      return(P)
}

# Get a list of unique places
uniquePlaces <- function(P) {
      P = toCharacter(P)
      if(class(P[,1]) != "character") P = P[, 2:dim(P)[2]]
      places = vector(mod="character", length=0)
      for(i in 1:dim(P)[1]) {
            for(j in 1:dim(P)[2]) {
                  if(P[i,j] != "" & !is.na(P[i,j]) & !is.null(P[i,j])) {
                        places = append(places, P[i,j])
                  }
            }
      }
      return(list(places = places, unique = unique(places)))
}

# This function takes a final place dataset and tranforms it to a place matrix
getPlaceMatrix <- function(P) {
      P = toCharacter(P)
      N = uniquePlaces(P)$unique # gets unique place names
      M = matrix(0, nrow = dim(P)[1], ncol = length(N)) # initialize a place matrix
      colnames(M) = N
      return(M)
}

# This function takes a final place dataset and gives a people-place adjacency matrix
getPeoplePlaceMatrix <- function(P) {
      require(dplyr)
      P = toCharacter(P)
      P = arrange(P, id)
      ids = P$id
      P = P[, -which(names(P) == "id")]
      AM = getPlaceMatrix(P) # gets a place matrix
      for(i in 1:dim(P)[1]) {
            for(j in 1:dim(P)[2]) {
                  if(P[i,j] != "" & !is.na(P[i,j]) & !is.null(P[i,j])) {
                        place = P[i,j]
                        AM[i, place] = 1 # double indications are not considered
                  }
            }
      }
      rownames(AM) = ids
      return(AM)
}

# Get the dominant type of cluster for the places and the entropy of the cluster type distribution; moreover it gets proportions of each cluster in places
getDominantClusters <- function(Pdat, D, P) {
      # Pdat is the dataset of place profiles; P is a final place dataset; D is a respondents dataset
      source("HelperFunctionsMisc/ComputingMisc.R")
      require(dplyr)
      P = arrange(P, id)
      D = arrange(D, id)
      if(!all.equal(P$id, D$id)) stop("ids in P and D are not equal")
      places = rownames(Pdat)
      dcluster = vector(mode="character", length=dim(Pdat)[1]) # dominant clusters
      ent = vector(mode="numeric", length=dim(Pdat)[1]) # absolute entropies
      r_ent = vector(mode="numeric", length=dim(Pdat)[1]) # vector of relative entropies
      cluster1 = vector(mode="numeric", length=dim(Pdat)[1]) # vector of proportions of the cluster 1 - Wolne Zawody
      cluster2 = vector(mode="numeric", length=dim(Pdat)[1]) # vector of proportions of the cluster 2 - Studenci
      cluster3 = vector(mode="numeric", length=dim(Pdat)[1]) # vector of proportions of the cluster 3 - Kulturalnie wycofani
      
      for(place in places) {
            cluster_dist = vector(mode="character", length=0)
            for(id in D$id) {
                  if(place %in% P[P$id==id, ]) {
                        cluster_type = as.character(D[D$id==id, "cluster"])
                        cluster_dist = append(cluster_dist, cluster_type)
                  }
            }
            mode = domin(cluster_dist) # mode of the cluster type distribution
            H = entropy(cluster_dist, rel = FALSE) # absolute entropy of the distribution
            Hr = entropy(cluster_dist, rel = TRUE) # relative entropy of the distribution
            ### cluster 1 - Wolne Zawody
            clust1 = table(cluster_dist)[3]/sum(table(cluster_dist))
            ### cluster 2 - Studenci
            clust2 = table(cluster_dist)[2]/sum(table(cluster_dist))
            ### cluster 3 - Kulturalnie Wycofani
            clust3 = table(cluster_dist)[1]/sum(table(cluster_dist))
            
            index = which(places == place) # get the index of the place
            dcluster[index] = mode # assign a dominant cluster to a place
            ent[index] = H # assign absolute entropy to a place
            r_ent[index] = Hr # assing relative entropy to a place
            cluster1[index] = clust1 # assign cluster 1 proportion to a place
            cluster2[index] = clust2 # assign cluster 2 proportion to a place
            cluster3[index] = clust3 # assign cluster 3 proportion to a place
      }
      Pdat$dcluster = as.factor(dcluster)
      Pdat$ent = ent
      Pdat$r_ent = r_ent
      Pdat$cluster1 = cluster1
      Pdat$cluster2 = cluster2
      Pdat$cluster3 = cluster3
      return(Pdat)
}

# This function computes average profiles (for numerical variables) of places
profilePlaces <- function(Pdat, D, P) {
      # Pdat is the dataset of place profiles; P is a final place dataset; D is a respondents dataset
      require(dplyr)
      P = arrange(P, id)
      D = arrange(D, id)
      ids = D$id # get a set of ids
      if(!all.equal(P$id, D$id)) stop("ids in P and D are not equal")
      places = rownames(Pdat)
      numvars = which(sapply(D, class) == "numeric")
      D = D[, numvars] # restrict the respondents dataset to numeric variables only
      D = cbind(id = ids, D) # add ids again
      PM = matrix(0, nrow=dim(Pdat)[1], ncol=dim(D)[2]-1)
      colnames(PM) = names(D)[-1] # variables names without id
      rownames(PM) = rownames(Pdat) # sets up a matrix of average place profiles
      for(place in places) {
            M = matrix(0, nrow=0, ncol=dim(D)[2]-1)
            colnames(M) = names(D)[-1] # a matrix of respondents profiles
            for(id in ids) {
                  if(place %in% P[P$id==id, ]) {
                        row = D[D$id==id, -1]
                        M = rbind(M, row)
                  }
            }
            avg_profile = apply(M, 2, mean, na.rm=TRUE)
            PM[place, ] = avg_profile
      }
      Pdat = cbind(Pdat, PM)
      return(Pdat)
}

# This function combines dominant clusters and average profiles
getFullPlaceInfo <- function(Pdat, D, P) {
      Q = getDominantClusters(Pdat, D, P)
      W = profilePlaces(Pdat, D, P)
      Pdat = cbind(Q, W[,-1])
      return(Pdat)
}

# This function computes various social space homogeneity indices based on place entropy distributions for individuals. These indices are:
# 1) sum of entropies; 2) average entropy; 3) maximal entropy; 4) minimal entropy
heterogeneityCoefs <- function(Pdat, D, P, threshold=0) {
      source("HelperFunctionsMisc/ComputingMisc.R")
      require(dplyr)
      D = arrange(D, id)
      P = arrange(P, id)
      P = toCharacter(P)
      places = rownames(Pdat)
      # initialize vectors of entropy measures
      ent_total = vector(mode="numeric", length=dim(D)[1])
      ent_avg = vector(mode="numeric", length=dim(D)[1])
      ent_wgh = vector(mode="numeric", length=dim(D)[1])
      ent_max = vector(mode="numeric", length=dim(D)[1])
      ent_min = vector(mode="numeric", length=dim(D)[1])
      for(id in D$id) {
            # initialize a vector for storing place entropies of an individual
            ent_vec = vector(mode="numeric", length=0)
            # initialize a vector for storing weights of places for an individual
            # places are wieghted in regard to square root of popularity
            # this is due to the fact that popular places should have higheer weights, but at the same time they should not completely dominate other places
            weights = vector(mode="numeric", length=0)
            for(place in places) {
                  if(Pdat[place, "popularity"] < threshold) next
                  if(place %in% P[P$id==id, ]) {
                        ent_vec = append(ent_vec, Pdat[place, "ent"])
                        weights = append(weights, sqrt(Pdat[place, 1]))
                  }
                  index = which(D$id == id) # for indexing entropy values on ent_ vectors
                  if(length(ent_vec) > 0) {
                        ent_total[index] = sum(ent_vec, na.rm=TRUE)
                        ent_avg[index] = mean(ent_vec, na.rm=TRUE)
                        ent_wgh[index] =  as.numeric(t(ent_vec %*% weights)) / sum(weights)
                        ent_max[index] = max(ent_vec, na.rm=TRUE)
                        ent_min[index] = min(ent_vec, na.rm=TRUE)
                  }
                  else {
                        ent_total[index] = NA
                        ent_avg[index] = NA
                        ent_wgh[index] = NA
                        ent_max[index] = NA
                        ent_min[index] = NA
                  }
            }
      }
      E = data.frame(ent_total, ent_avg, ent_wgh, ent_max, ent_min)
      return(E)
}

# Real entropy measure; it is computed as entropy of the distribution of all (unique) persons (in terms of their cluster assignments) that are met through places that are being visited by the respondent

realEntropy <- function(D, AM, rel=FALSE) {
      source("HelperFunctionsMisc/ComputingMisc.R")
      require(dplyr)
      ids = as.numeric(rownames(AM))
      ids = sort(ids)
      D = arrange(D, id)
      places = colnames(AM)
      AM = AM[as.character(ids), ]
      # initialize a vector storing real entropy values
      ent = vector(mode="numeric", length=dim(D)[1])
      for(id in ids) {
            char.id = as.character(id)
            index = which(ids == id) # to index new values properly on the ent vector
            placevec = places[AM[char.id, ] == 1]
            distentropy = 0 # initialize distribution entropy
            if(length(placevec) > 0) {
                  personvec = apply(as.matrix(AM[, placevec]), 1, sum)
                  personvec[personvec > 1] = 1 # to includ only unique persons
                  persons.id = ids[personvec == 1]
                  clusterdist = D[D$id %in% persons.id, "cluster"]
                  distentropy = entropy(clusterdist, rel=rel)
                  ent[index] = distentropy
            }
            else {
                  distentropy = NA
                  ent[index] = distentropy
            }
      }
      return(ent)
}
            