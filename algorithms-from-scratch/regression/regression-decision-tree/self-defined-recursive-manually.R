# data
library('MASS')
set.seed(1)
train   <- sample(1:nrow(Boston), nrow(Boston) / 2)
X_train <- unname(as.matrix(Boston[train, c('rm', 'dis')]))
y_train <- unname(as.matrix(Boston[train, c('medv')]))


# unlike use X and y in many cases, I use X_train and y_train
# since there is also a parameter called X in lapply()

selectFeature <- function(X_train) {
  # randomly select one features
  # @param X_train, numerical matrix
  
  n_features <- ncol(X_train)
  feature_idx <- sample(seq(n_features), size = 1, replace = FALSE)
  return(feature_idx)
}

splitFeature <- function(X_train, feature_idx, length = 20) {
  # get all split points of one feature
  # @param X_train, numerical matrix
  # @param feature_idx, integer
  #   index of selected feature
  # @param length, integer
  #   total number of split intervals
  
  feature <- X_train[, feature_idx]
  splits  <- seq(min(feature), max(feature), length.out = length)
  return(splits)
}
  
splitAllFeature <- function(X_train, length = 20) {
  # split all features
  # @param X_train, numerical matrix
  # @param length, integer
  
  all_splits <- unlist(lapply(seq(ncol(X_train)), splitFeature, X_train = X_train, length = length))
  all_splits <- matrix(all_splits, nrow = length, ncol = ncol(X_train))
  return(all_splits)
}

costFunc <- function(X_train, y_train, feature_idx, split_point) {
  # calculate the cost function after one split
  # @param X_train, numerical matrix
  # @param y_train, numerical vector
  # @param feature_idx, integer
  # @param split_point, numerical 
  
  R_i <- X_train[, feature_idx] <= split_point
  R_j <- X_train[, feature_idx] >  split_point
  return(sum((y_train[R_i] - mean(y_train[R_i]))^2) + sum((y_train[R_j] - mean(y_train[R_j]))^2))
}

calcCost <- function(all_splits, X_train, y_train) {
  # calculate cost along all combination of feature - split
  # @param all_splits, numerical matrix
  # @param X_train, numerical matrix
  # @param y_train, numerical vector
  
  n_features <- ncol(all_splits)
  all_costs  <- matrix(NA, nrow = nrow(all_splits), ncol = ncol(all_splits)) 
  for (i in seq(n_features)) {
    all_costs[, i] <- unlist(lapply(all_splits[, i], costFunc, X_train = X_train, y_train = y_train, feature_idx = i))
  }
  return(all_costs)
}

getSplit <- function(all_splits, all_costs) {
  # get the feature - split pair which minimize the cost 
  # @param all_splits, numerical matrix
  # @param all_cost, numerical matrix
  
  split_idx <- which.min(all_costs) %% nrow(all_costs)
  feature_idx <- which.min(all_costs) %/% nrow(all_costs) + 1
  return(list(feature_idx = feature_idx, split = all_splits[split_idx, feature_idx]))
}


# manually implement the "recursive" process
# first split
all_splits_1 <- splitAllFeature(X_train, length = 20)
all_costs_1  <- calcCost(all_splits_1, X_train, y_train)
split_node_1 <- getSplit(all_splits_1, all_costs_1)
R1 <- which(X_train[, split_node_1$feature_idx] <= split_node_1$split)
R2 <- which(X_train[, split_node_1$feature_idx] > split_node_1$split)

# second split
all_splits_2 <- splitAllFeature(X_train[R1, ], length = 20)
all_costs_2  <- calcCost(all_splits_2, X_train[R1, ], y_train[R1])
split_node_2 <- getSplit(all_splits_2, all_costs_2)
R3 <- which(X_train[R1, split_node_2$feature_idx] <= split_node_2$split)
R4 <- which(X_train[R1, split_node_2$feature_idx] > split_node_2$split)

# third split
all_splits_3 <- splitAllFeature(X_train[R2, ], length = 20)
all_costs_3  <- calcCost(all_splits_3, X_train[R2, ], y_train[R2])
split_node_3 <- getSplit(all_splits_3, all_costs_3)
R5 <- which(X_train[R2, split_node_3$feature_idx] <= split_node_3$split)
R6 <- which(X_train[R2, split_node_3$feature_idx] > split_node_3$split)

# three nodes: root and its two children
split_node_1
split_node_2
split_node_3
