# required package
library(numDeriv)
# get the gradident of function at x0
#
#   grad(f, x0)

gradientDescent <- function(x, func, stop_val = 1e-6, step = 0.01, max_iter = 1000, ...) 
{
  # @param x: the args that minimize objective function
  # @param func: objective function
  # @param stop_deriv: stop criteria
  # @param step: learning rate
  # @param max_iter: maximum iterations
  
  # initialization
  iter <- 1
  x_iter <- matrix(0, nrow = length(x), ncol = max_iter)
  f_iter <- rep(0, max_iter)
  x_iter[, 1] <- x
  f_iter[1] <- func(x)
  
  for (iter in 2:max_iter) {
    
    # calculate gradient
    current_grad <- grad(func, x_iter[, iter - 1], ...)
    
    # update
    x_iter[, iter] <- x_iter[, iter - 1] - step * current_grad
    f_iter[iter] <- func(x_iter[, iter])
    
    # check stop criteria
    if (abs(f_iter[iter] - f_iter[iter - 1]) < stop_val) {
      iter <- iter - 1
      break
    }
  }
  
  return(list(opt = x_iter[, iter], x_iter = x_iter[, 1:iter], f_iter = f_iter[1:iter]))
}

# Simple demo
f <- function(x) (x[1] - 3.191)^2 + (x[2] + 11.231)^2 
x_init <- c(0, 0)
gradientDescent(x = x_init, func = f)

# Apply on simple linear regression
set.seed(123)
x <- runif(40, 10, 50)
y <- 1.34 + 3.21 * x + rnorm(40, sd = 3)
df <- data.frame(y = y, x = x)
lm(y ~ x, data = df)

X <- cbind(1, df[, 2, drop = TRUE])

costFunc <- function(theta) {
  cost <- t(y - X %*% theta) %*% (y - X %*% theta) / (2 * nrow(X))
  return(cost)
}

costFunc( c(1.132, 3.217) )

theta <- c(0.5, 0)
costFunc(theta)

lm_result <- gradientDescent(theta, costFunc, stop_val = 1e-6, step = 0.001, max_iter = 10000)  # params here are very important
length(lm_result$f_iter)
lm_result$opt
plot(lm_result$f_iter, type = 'p', xlab = 'iter', ylab = 'cost function')
lines(lm_result$f_iter)
