# This script provide some helper function for MCA
# it is primary goal is to provide function for projecting individuals into a soluton space

getCoordinates <- function(mca, k=2) {
      # this function derives variable coordinates from a mjca object
      coord = data.frame(category=mca$levelnames)
      coord = cbind(coord, mca$rowcoord[,1:k], mca$rowpcoord[,1:k])
      N = vector(mode="character", length=k*2)
      for(i in 1:k) {
            N[i] = paste("s.dim", i, sep="") # gets standard coordinates
      }
      for(i in (k+1):(k*2)) {
            N[i] = paste("p.dim", i-k, sep="") # gets principal coordinates
      }
      names(coord)[2:(k*2+1)] = N
      return(coord)
}      

getZ <- function(D, mca) {
      # this fuction derives an indicator matrix using data object and mjca object
      columns = mca$levelnames
      n = dim(D)[1]
      m = dim(D)[2] # dimensions of the data frame
      k = length(columns) # number of columns of Z
      Z = matrix(0, nrow=n, ncol=k, dimnames=list(1:n, columns)) # initialize Z
      for(i in 1:n) {
            for(j in 1:m) {
                  ans = D[i, j] # respondent's answer
                  col = paste(names(D)[j], ans, sep=":")
                  Z[i, col] = 1
            }
      }
      return(Z)
}


# This function project respondents into a solution space (in either standard or principal coordinates)
projectInd <- function(D, mca, principal=TRUE, k=2) {
      # it takes dataset and a mjca object as arguments. The third argument correspond to which coordinates (standard or principle) should be computed. The default is principal. The fourth argument says how many dimensions should be included; the default is 2.
      coords.df = getCoordinates(mca, k) # get categories' coordinates as a data frame together with the names of the categories
      sv = mca$sv # gets singular values
      Z = getZ(D, mca) # get an indicator matrix Z
      coords = 0
      if(!principal) coords = coords.df[, 2:(k+1)]
      else coords = coords.df[, (1+k+1):(1+2*k)] # gets either principal or standard coordinates
      coords = as.matrix(coords) # saves them as a matrix
      ind.coords = (Z %*% coords) / dim(D)[2]
      for(i in 1:k) ind.coords[,i] = ind.coords[,i] / sv[i]
      return(as.data.frame(ind.coords))
}

# This function plots mjca object in a good way
makeMCAplot <- function(D, mca, principal=TRUE, colors=c("red", "blue"), labels=TRUE, center=TRUE, zoomout=2, cex=.5, lines=TRUE, limsym=TRUE, ...) {
      coord = getCoordinates(mca)
      categories = coord[,1] # gets categories' names
      k = length(categories)
      n = dim(D)[1] # number of categories and respondents
      if(principal) coord = coord[,4:5]
      else coord = coord[,2:3] # gets appropriate coordinates
      ind.coord = projectInd(D, mca, principal) # gets persons' coordinates
      all.coord = rbind(coord, ind.coord)
      all.coord = cbind(all.coord, color = c(rep(colors[1], k), rep(colors[2], n)))
      all.coord = cbind(all.coord, shape = c(rep(17, k), rep(1, n)))
      all.coord = cbind(all.coord, size = c(rep(1, k), rep(.5, n)))
      xmax = max(all.coord[,1], na.rm=T)
      ymax = max(all.coord[,2], na.rm=T)
      if(center) {
            xmax = zoomout*max(all.coord[1:k, 1], na.rm=T)
            ymax = zoomout*max(all.coord[1:k, 2], na.rm=T)
      }
      max = 0
      if(limsym) {
            max = max(c(xmax, ymax))
            xmax = max
            ymax = max
      }
      P = plot(all.coord[,1:2], col=all.coord[,3], pch=all.coord[,4], cex=all.coord[,5], xlim=c(-xmax, xmax), ylim=c(-ymax, ymax), ...)
      abline(h=0, lwd=.8, lty=2)
      abline(v=0, lwd=.8, lty=2)
      text.df = all.coord[1:k, 1:2]
      text.df = cbind(text.df, labels=categories)
      if(labels) text(text.df[,1:2], labels=text.df[,3], cex=cex)
      lev = mca$levels.n
      row = 1
      for(i in 1:length(lev)) {
            l = lev[i]
            lines(all.coord[row:(row-1+l), 1:2], lwd=.7, lty=3)
            row = row + l
      }
      return(P)
}
      
      
      