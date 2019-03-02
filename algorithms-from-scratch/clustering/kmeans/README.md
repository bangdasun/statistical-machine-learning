### Kmeans clustering
K-means clustering algorithm is a special case of EM algorithm,  which is one of algorithms to estimate the parameter of mixture model, it also can be viewed as a special kind of clustering. The distance is usually Euclide distance.


1. Initialize the assignment of cluster to every sample
2. while (prev_cluster != curr_cluster):
      * calculate the center of each cluster (use mean)
      * calculate the distance (Euclide distance) of every sample to all centers
      * assign the distance to the nearest center
