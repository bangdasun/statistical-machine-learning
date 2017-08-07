# Data
x = rnorm(20, mean = 3, sd = 1)
y = rnorm(20, mean = 4, sd = 2)
y[x < 3] = y[x < 3] + 10
df = data.frame(x = x, y = y)
library(ggplot2)
ggplot(df) + geom_point(mapping = aes(x = x, y = y), size = 2)

# Clustering
km = kmeans(df, centers = 2, nstart = 10)
km

df$cluster1 = factor(km$cluster)
ggplot(df) + geom_point(mapping = aes(x = x, y = y, colour = cluster1), size = 2)
