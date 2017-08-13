# Data
set.seed(123)
x <- runif(40, 10, 50)
y <- 1.34 + 3.21 * x + rnorm(40, sd = 3)
df <- data.frame(y = y, x = x)
X <- cbind(1, df[, 2, drop = TRUE])

costFunc <- function(theta) {
  cost <- t(y - X %*% theta) %*% (y - X %*% theta) / (2 * nrow(X))
  return(cost)
}

# built-in function (as validation)
linReg_model <- lm(y ~ x, data = df)

# use gradient descent to find minimum of cost function (OLS idea)
theta <- c(0.5, 0)
lm_result <- gradientDescent(theta, costFunc, stop_val = 1e-6, step = 0.001, max_iter = 10000)

# compare results
with(df, plot(x, y))
abline(linReg_model, col = 'red')
abline(a = lm_result$opt[1], b = lm_result$opt[2], lty = 'dashed', col = 'blue')
