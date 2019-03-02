### use tree package
library("tree")

# data
df <- data.frame(
  y = as.vector(y_train),
  x1 = as.vector(X_train[, 1]),
  x2 = as.vector(X_train[, 2])
)

# fit a naive decision tree
dc_tree <- tree(y ~ x1 + x2, data = df)
summary(dc_tree)
plot(dc_tree)
text(dc_tree, pretty = 0)

# rmse
y_pred <- predict(dc_tree, df)
sqrt(mean((y_pred - df$y)^2))


### use rpart package
library("rpart")

dc_tree_rpart <- rpart(y ~ x1 + x2, df)
summary(dc_tree_rpart)
plot(dc_tree_rpart)
text(dc_tree_rpart, pretty = 0)


class(dc_tree) # tree class
class(dc_tree_rpart) # rpart class
