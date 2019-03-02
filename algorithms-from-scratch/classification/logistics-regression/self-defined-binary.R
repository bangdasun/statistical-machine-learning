# run required script:
# https://github.com/bangdasun/Statistical-machine-learning/blob/master/algorithms/clustering/kmeans/self-defined.R

# Data
colnames(df) <- c('x1', 'x2', 'y')
df$c <- 1
df$y <- ifelse(df$y == 1, 0, 1)
df <- df[c('c', 'x1', 'x2', 'y')]  # re-order

X <- as.matrix(df[, -4])
y <- as.matrix(df[, 4])

# negative log-likelihood function
logLikehd <- function(beta) {
  # @param: beta
  
  num_params <- length(beta)
  n <- nrow(X)
  
  first_part <- 0
  for (i in 1:n) {
    first_part <- first_part - log((exp(-X[i, ] %*% beta)) / (1 + exp(-X[i, ] %*% beta)))
  }
  return(as.vector(first_part - t(y) %*% (X %*% beta)))
}

# built-in function (as validation, return some warnings, probably is the issue of data)
logReg_model <- glm(y ~ x1 + x2, data = df, family = binomial)
slope_builtin <- -coef(logReg_model)[2] / coef(logReg_model)[3]
intercept_builtin <- -coef(logReg_model)[1] / coef(logReg_model)[3]

# use gradient descent to find MLE of negative log-likelihood function
beta_init <- c(60, -30, 0)  # different initial values will yield different results, issue of data
logReg_result <- gradientDescent(beta_init, logLikehd, stop_val = 1e-6, step = 0.001, max_iter = 10000)
slope_gd <- -logReg_result$opt[2] / logReg_result$opt[3]
intercept_gd <- -logReg_result$opt[1] / logReg_result$opt[3]

# compare results
library(dplyr)
df %>% 
  mutate(y = factor(y)) %>%
  ggplot() + 
  geom_point(aes(x = x1, y = x2, col = y), size = 2) + 
  geom_abline(aes(col = 'built-in', slope = slope_builtin, intercept = intercept_builtin), linetype = 'solid', size = 2) + 
  geom_abline(aes(col = 'GD on LL', slope = slope_gd, intercept = intercept_gd), linetype = 'dashed', size = 2)
