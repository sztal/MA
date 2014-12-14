# These are helper functions for the "Kmeans.R" script
# It is based on: http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/

wssplot <- function(data, nc=15, seed=5050) {
      wss = (nrow(data)-1) * sum(apply(data, 2, var))
      for(i in 2:nc) {
            set.seed(seed)
            wss[i] <- sum(kmeans(data, centers=i, iter.max=40, nstart=50)$withinss)
      }
      plot(1:nc, wss, type="b", xlab="Number of clusters", ylab = "Within groups sum of squares")
}