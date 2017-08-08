# required package
library(numDeriv)
# get the gradident of function at x0
#
#   grad(f, x0)

gradientDescent <- function(x, func, threshold = 1e-6, step = 0.01, max_iter = 1000, ...) 
{
  # @param x: the args that minimize objective function
  # @param func: objective function
  # @param threshold: stop criteria
  # @param step: learning rate
  # @param max_iter: maximum iterations
  
  # initialization
  i <- 1
  all_x <- matrix(0, nrow = length(x), ncol = 1)
  all_x[, 1] <- x
  all_f <- func(x)
  
  while (i < max_iter) {
    i <- i + 1
    
    # update x
    grad_f <- grad(func, x, ...)
    x <- x - step * matrix(grad_f, ncol = 1)
    
    # record values
    all_x <- cbind(all_x, x)
    all_f <- c(all_f, func(x))
    
    # check stop criteria
    if (abs(func(all_x[, i - 1]) - func(x)) < threshold) {
      print('optimized value found.')
      break
    }
    
    if (i == max_iter) stop('Maximum iterations exceeded, try to adjust parameters.')
  }

  return(list(opt = x, x_iter = all_x, f_iter = all_f))
}

# Simple demo
f <- function(x) (x[1] - 3.191)^2 + (x[2] + 11.231)^2 
x_init <- c(0, 0)
gradientDescent(x_init, f)
