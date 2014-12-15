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