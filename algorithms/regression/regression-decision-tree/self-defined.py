import os
import numpy as np
import pandas as pd

class Node:
    """
    node on decision tree
    @param feature_idx
      represent the selected feature for the node
    @param obs_idx
      represent the data that the node split based on
    @param split
      split point, data was divided into X_j <= split and X_j > split
    """
    def __init__(self, X, y, feature_idx, obs_idx, split):
        self.X = X
        self.y = y
        self.feature_idx = feature_idx
        self.obs_idx = obs_idx
        self.split = split
        
    def inspect_child(self):
        R_i = self.X[self.obs_idx, self.feature_idx] <= self.split
        R_j = self.X[self.obs_idx, self.feature_idx] > self.split
        return np.mean(self.y[R_i]), np.mean(self.y[R_j])
    
    def split_rss(self):
        R_i = self.X[self.obs_idx, self.feature_idx] <= self.split
        R_j = selfX[self.obs_idx, self.feature_idx] > self.split
        return np.sum((self.y[R_i] - np.mean(self.y[R_i])) ** 2) + np.sum((self.y[R_j] - np.mean(self.y[R_j])) ** 2)
    
    def count_sample(self):
        R_i = self.X[self.obs_idx, self.feature_idx] <= self.split
        R_j = self.X[self.obs_idx, self.feature_idx] > self.split
        return (np.sum(R_i), np.sum(R_j))
    
    def get_sample_idx(self):
        R_i = self.X[self.obs_idx, self.feature_idx] <= self.split
        R_j = self.X[self.obs_idx, self.feature_idx] > self.split
        return [R_i, R_j]
    
    def to_leaf(self):
        return np.mean(self.y)


class RegressionTree:
    """
    decision tree
    @param max_depth
      control the size of the tree
    @param cost_func
      represent the data that the node split based on
    @param nodes
      list of nodes to store the node of decision tree
    """
    def __init__(self, max_depth, min_split_sample, cost_func):
        self.max_depth = max_depth
        self.min_split_sample = min_split_sample
        self.cost_func = cost_func
        self.nodes = []
    
    def get_root(self, X, y, length = 20):
        # use broadcasting to get split points
        feature_min = np.amin(X, axis = 0)
        feature_max = np.amax(X, axis = 0)
        steps = (1. / (length - 1)) * (feature_max - feature_min)
        all_splits = (steps[:, None] * np.arange(length) + feature_min[:, None]).T
        
        # calculate cost for each split
        all_costs = np.zeros((length, X.shape[1]))
        for feature in range(X.shape[1]):
            for split_idx in range(all_splits.shape[0]):
                all_costs[split_idx, feature] = cost_func(X, y, feature, all_splits[split_idx, feature])
        
        min_cost = all_costs.argmin()
        
        # create root node
        root = Node(X = X, y = y, feature_idx = min_cost % all_costs.shape[1], 
             obs_idx = [True] * X.shape[0],
             split = all_splits[min_cost / all_costs.shape[1], min_cost % all_costs.shape[1]])
        return root

    def recursive_split(self, node, X, y, current_depth):
        left_idx, right_idx = node.get_sample_idx()
        left_depth, right_depth = current_depth, current_depth
        left = self.get_root(X[left_idx, :], y[left_idx])
        right = self.get_root(X[right_idx, :], y[right_idx])       
        
        if np.sum(left_idx) + np.sum(right_idx) <= self.min_split_sample:
            node.to_leaf()
            return 
        
        if np.max([left_depth, right_depth]) > self.max_depth:
            left.to_leaf()
            right.to_leaf()
            return
            
        
        if np.sum(left_idx) >= self.min_split_sample:
            self.nodes.append(left)
            self.recursive_split(left, X[left_idx, :], y[left_idx], left_depth + 1)
        else:
            left.to_leaf()
            return
        
        if np.sum(right_idx) >= self.min_split_sample:
            self.nodes.append(right)
            self.recursive_split(right, X[right_idx, :], y[right_idx], right_depth + 1)
        else:
            right.to_leaf()
            return
			

os.chdir('C:\Users\Bangda\Desktop')
df = pd.read_csv('df.csv')
X_train = df.iloc[:, 1:].values
y_train = df.iloc[:, 0].values

def cost_func(X, y, feature_idx, split):
    R_i = X[:, feature_idx] <= split
    R_j = X[:, feature_idx] > split
    return np.sum((y[R_i] - np.mean(y[R_i])) ** 2) + np.sum((y[R_j] - np.mean(y[R_j])) ** 2)

tree = RegressionTree(max_depth = 3, min_split_sample = 10, cost_func = cost_func)
root = tree.get_root(X_train, y_train, length = 20)
root.feature_idx, root.split # root split

tree.recursive_split(root, X_train, y_train, 1)