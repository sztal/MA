# These are various computing functions of general utility

# Dominant value of a vector
domin <- function(x) {
      Tab = table(x)
      mode = 0
      uni = 0
      if(is.numeric(x)) {
            uni = sort(unique(x))
            mode = which(Tab==max(Tab))[[1]]
            mode = uni[mode]
            return(mode)
      }
      else {
            mode = which(Tab==max(Tab))[[1]]
            return(names(Tab[mode]))
      }
}

# Transforms extreme outliers into NAs
OLsToNAs <- function(x, k=5) {
      if(!is.numeric(x)) stop("data is not numeric")
      sigma = sd(x, na.rm=T)
      mu = mean(x, na.rm=T)
      x[abs(x-mu) > (k*sigma)] = NA
      return(x)
}

# Computes standard error of a vector mean
se <- function(x, na.rm=FALSE) return(sd(x, na.rm=na.rm) / sqrt(length(x)))

# Relative variance
relvar <- function(x, na.rm=FALSE) {
      L = max(x, na.rm=na.rm) - min(x, na.rm=na.rm)
      D = var(x, na.rm=na.rm)
      return(D / (L^2/4))
}

# Make an evaluation expression of a form: var1 + var2 + ... + varn
expr <- function(data, vars=names(data), sep = "+") {
      if(is.numeric(vars)) vars = names(data)[vars]
      expr = paste("~", vars[1], sep=" ")
      for(var in vars[-1]) {
            expr = paste(expr, var, sep=sep)
      }
      return(as.formula(expr))
}

# Computes entropy (in bits) of a distribution (absolute or relative)
entropy <- function(x, rel = FALSE) {
      # x maybe of a class table or or a simple vector
      if(class(x) != "table") x = table(x)
      x = prop.table(x)
      if(length(which(x==0)) != 0) x = x[-which(x==0)]
      k = length(x) # number of classes
      H = sum(-x*log(x, 2))
      if(rel) {if(k==1) return(0) else return(H / log(k, 2))}
      else return(H)
}
# This function centers or normalize a numeric variable vector
center <- function(x, norm=FALSE, na.rm=TRUE) {
      if(norm) {
            x = (x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
      }
      else x = x - mean(x, na.rm=na.rm)
      return(x)
}

makeGrid <- function(x, y, len=length(x)) {
      x = sort(rep(x, len))
      y2 = y
      y = vector(mode="numeric", length=len^2)
      for(i in 1:len) y[((i-1)*len+1):(i*len)] = y2
      return(cbind(x, y))
}