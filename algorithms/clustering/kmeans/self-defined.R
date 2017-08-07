# Data
set.seed(1234)
x = rnorm(40, mean = 3, sd = 1)
y = rnorm(40, mean = 4, sd = 2)
y[x < 3] = y[x < 3] + 10
X <- cbind(x, y)
df <- as.data.frame(X)
library(ggplot2)
ggplot(df) + geom_point(mapping = aes(x = x, y = y), size = 2)

# Help functions
calcDistance <- function(x, center) {
  # @param x: sample
  # @param center: center of cluster
  if (length(x) != length(center)) stop('x and center should have same length.')
  
  x      <- matrix(x, nrow = 1)
  center <- matrix(center, nrow = 1)
  return( (x - center) %*% t((x - center)) )
}

assignCluster <- function(x, Center) {
  # @param x: sample
  # @param Center: with shape k x p
  dist <- rep(0, nrow(Center))
  for (i in 1:nrow(Center)) {
    dist[i] <- calcDistance(x, Center[i, ])
  }
  return( which.min(dist) )
}

calcCenter <- function(X, cluster) {
  # @param X: data matrix
  # @param cluster: vector of cluster assignment
  Center <- matrix(0, nrow = length(unique(cluster)), ncol = ncol(X))
  for (i in unique(cluster)) {
    Center[i, ] <- colMeans(X[which(cluster == i), ])
  }
  return(Center)
}

# Main functions
my_kmeans <- function(X, k = 2) {
  # @param X: data matrix with shape n x p 
  # @param k: number of clusters
  
  Center <- matrix(0, nrow = k, ncol = ncol(X))
  clusterAssignMat <- matrix(0, nrow = nrow(X), ncol = 1)
  prevCluster <- clusterAssignMat
  
  currentCluster <- sample(1:k, size = nrow(X), replace = TRUE)
  
  while(!identical(prevCluster, currentCluster)) {
    clusterAssignMat <- cbind(clusterAssignMat, currentCluster)
    prevCluster <- currentCluster
    # check again if cluster unchanged
    if (identical(prevCluster, currentCluster)) {
      break
    }
    Center <- calcCenter(X, currentCluster)
    currentCluster <- apply(X, 1, assignCluster, Center = Center)
  }
  return(list(cluster = currentCluster, Center = Center, cluster_iter = clusterAssignMat))
}

# Demo
kmeansResult <- my_kmeans(X, k = 2)
df$cluster <- kmeansResult$cluster
ggplot(df) + geom_point(mapping = aes(x = x, y = y, color = factor(cluster)), size = 2)
