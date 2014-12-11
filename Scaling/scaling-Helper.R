meanSort <-function(DF) {
      rDF = DF
      cols = dim(DF)[2]
      means = apply(DF, 2, mean, na.rm=TRUE)
      id = 1:cols
      ctrl = data.frame(id=id, xbar=means)
      ctrl = ctrl[order(means), ]
      for(i in 1:cols) {
            rDF[, i] = DF[, ctrl[i,1]]
            names(rDF)[i] = names(DF)[ctrl[i,1]]
      }
      return(rDF)
}

# Function for computing guttman errors
gutmax <- function(k) {
      guterr <- function(k, pos) {
            x = vector("numeric", length=k)
            x[pos] = 1
            ones = which(x==1)
            gut = 0
            for(i in ones) {
                  if(i > 1) {
                        for(j in 1:(i-1)) {
                              if(x[j] == 0) gut = gut + 1
                        }
                  }
            }
      return(gut)
      }
      errors = vector(mode="numeric", length=k)
      for(i in k:1) {
            errors[i] = guterr(k, i:k)
      }
      return(max(errors))
}