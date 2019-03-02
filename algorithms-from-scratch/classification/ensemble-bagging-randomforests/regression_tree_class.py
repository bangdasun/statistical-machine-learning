
# coding: utf-8

# In[1]:

class Node:
    def __init__(self):
        self.feature_idx = np.inf
        self.split = np.inf
        self.loss = np.inf
        self.child_idx = [np.inf, np.inf]
        self.left = None
        self.right = None
        
    def _squared_loss(y, left, right):
        """
        calculate squared loss for a split
        """
        return np.sum((y[left] - np.mean(y[left])) ** 2) + np.sum((y[right] - np.mean(y[right])) ** 2)
        
    def _split_feature(self, X, y, feature_idx, split):
        """
        split a data set on an attribute and an attribute value
        """
        left, right = list(), list()
        for row_idx in range(X.shape[0]):
            if X[row_idx, feature_idx] < split:
                left.append(row_idx)
            else:
                right.append(row_idx)
        
        return left, right
    
    def get_optim_split(self, X, y):
        """
        search for the best split
        """
        for feature_idx in range(X.shape[1]):
            for row_idx in range(X.shape[0]):
                left, right = self._split_feature(X, y, feature_idx, X[row_idx, feature_idx])
                cost = self._sqaured_loss(y, left, right)
                if cost < self.loss:
                    self.feature_idx, self.split, self.loss, self.child_idx = feature_idx, X[row_idx, feature_idx], cost, [left, right]


# In[2]:

class RegressionTree:
    def __init__(self):
        self.root = Node()
        
    def _to_leaf(self, y):
        return np.mean(y)
    
    def _split(self, X, y, node, max_depth, min_split_sample, depth):
        left, right = node.child_idx
    
        if not left or not right:
            node.left = node.right = self._to_leaf(y)
    
        if depth > max_depth:
            node.left, node.right = self._to_leaf(y[left]), self._to_leaf(y[right])
        
        if len(left) <= min_split_sample:
            node.left = self._to_leaf(y[left])
        else:
            node.left = Node()
            node.left.get_optim_split(X[left, :], y[left])
            self._split(X[left, :], y[left], node.left, max_depth, min_split_sample, depth + 1)
        
        if len(right) <= min_split_sample:
            node.right = self._to_leaf(y[right])
        else:
            node.right = Node()
            node.right.get_optim_split(X[right, :], y[right])
            self._split(X[right, :], y[right], node.right, max_depth, min_split_sample, depth + 1)
    
    def build_tree(self, X, y, max_depth, min_split_sample):
        self.root.get_optim_split(X, y)
        self._split(X, y, self.root, max_depth, min_split_sample, 1)
    
    def print_tree(self, node, depth = 0):
        if isinstance(node, Node):
            print('{}[X{} < {}]'.format((depth * '-'), node.feature_idx, node.split))
            self.print_tree(node.left, depth + 1)
            self.print_tree(node.right, depth + 1)
        else:
            print('{}[{}]'.format(depth * '-', node))
