# This script provide various methods for network analysis and power-law testing

# This function performs logarithmic binning as described in: Fronczak and Fronczak, 2009
# For now, it assumes that the minimal value of x is 1
binLog <- function(x, base=10, log=TRUE) {
      xmax = max(x)
      upper = log(xmax, base) # solution of: base^x >= max(x)
      upper = ceiling(upper) # the first integer that satisfies the equation
      bin = c(0, base^(0:upper)) # binning cuts
      bin = cut(x, bin)
      means = as.numeric(sapply(by(x, bin, mean, na.rm=TRUE), mean))
      bin = prop.table(table(bin))
      if(log) {
            bin = log(bin)
            names(bin) = as.character(log(means, base))
            return(bin)
      }
      else {
            names(bin) = as.character(means)
            return(bin)
      }
}

# This function maps entries in probability distribution table to a vector of values
mapPr <- function(x) {
      tab = prop.table(table(x))
      values = as.numeric(names(tab))
      len = length(x)
      xpr = vector(mode="numeric", length=len)
      for(i in 1:len) {
            val = as.character(x[i])
            index = which(names(tab) == val)
            xpr[i] = tab[index]
      }
      return(xpr)
}

# This function randomly permutes edges of vertices in a graph; for technical reasons it takes only adjacency matrices as arguments (normal matrix object)
permuteEdges <- function(AM) {
      diagonal = diag(AM)
      n = dim(AM)[1]
      m = dim(AM)[2]
      for(i in 1:(n-1)) {
            mapvec = (i+1):n
            len = length(mapvec)
            vec = as.numeric(AM[i, mapvec])
            if(len > 1) permvec = sample(vec, size = len, replace = FALSE)
            else permvec = vec
            AM[i, mapvec] = permvec
            zerovec = 1:i
            AM[i, zerovec] = 0
      }
      AM[n, ] = 0
      AM = AM + t(AM) + diag(diagonal)
      return(AM)
}
            
            

# This function uses permuteEdges routine to compute nonparametric confidence interval for statistics based on edgelist of a graph
permtest <- function(AM, n=1000, FUN, ...) {
      require(igraph)
      statvec = vector(mode="numeric", length=n)
      for(i in 1:n) {
            tmp = permuteEdges(AM)
            graph = graph.adjacency(tmp, mode="undirected", weighted=TRUE, diag=FALSE, add.rownames=TRUE)
            stat = FUN(graph, ...)
            statvec[i] = stat
      }
      CI = quantile(statvec, probs = c(.05, .95))
      return(statvec)
}
      