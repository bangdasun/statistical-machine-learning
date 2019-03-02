setwd("C://Users//Bangda//Desktop//GR5241//hw6")
graphdata = read.table("graph.txt")
# A - transition matrix
A = matrix(0, ncol = 100, nrow = 100)
# C - adjacent matrix
C = matrix(0, ncol = 100, nrow = 100)
# Assign value to C and A
for (i in 1:1024){
  row = graphdata[i, 1]
  col = graphdata[i, 2]
  C[row, col] = 1
}
A = t(C / replicate(100, rowSums(C)))
# Iteration
k     = 40
n     = 100
alpha = 0.2
r     = matrix(1 / n, ncol = k + 1, nrow = n)
for (i in 2:(k + 1)){
  r[, i] = rep(alpha / n, n) + (1 - alpha) * A %*% r[, i - 1]
}
# Top 5 scores
(topscore = sort(r[, 41], decreasing = TRUE)[1:5])
# Top 5 ids
(topid    = order(r[, 41], decreasing = TRUE)[1:5])
# Low 5 scores
(lowscore = sort(r[, 41], decreasing = FALSE)[1:5])
# Low 5 ids
(lowid    = order(r[, 41], decreasing = FALSE)[1:5])

x = seq(0, 4, by = .01)
p = dexp(x, rate = 1)
plot(x, p, type = "l",
     xlab = "x", ylab = "p(x)",
     main = "P.d.f. of Exp(1)")
     
plot(x, p, type = "l",
     xlab = "x", ylab = "p(x)",
     main = "P.d.f. of Exp(1)")
points(x = 1, y = dexp(1, rate = 1), type = "p")
points(x = 2, y = dexp(2, rate = 1), type = "p")
points(x = 4, y = dexp(4, rate = 1), type = "p")

plot(x, p, type = "l",
     xlab = "x", ylab = "p(x)",
     main = "P.d.f. of Exp distribution with rate range from 1 to 5", lty = 1)
lines(x, dexp(x, rate = 2), type = "l", lty = 2)
lines(x, dexp(x, rate = 3), type = "l", lty = 3)
lines(x, dexp(x, rate = 4), type = "l", lty = 4)
lines(x, dexp(x, rate = 5), type = "l", lty = 5)
abline(v = 1)
abline(v = 2)
abline(v = 4)
legend("topright", legend = c("Exp(1)", "Exp(2)", "Exp(3)", "Exp(4)", "Exp(5)"),
       lty = 1:5)
       
set.seed(1)
sampleSize = 256
expSample = rexp(sampleSize, rate = 1)
alpha0 = 2
beta0 = .2
n = c(4, 8, 16, 256)
x = seq(0.01, 4, by = .01)
gamma = matrix(0, nrow = 4, ncol = length(x))
for (i in 1:4){
  alpha = alpha0 + n[i]
  beta = beta0 + sum(expSample[1:n[i]])
  gamma[i, ] = dgamma(x, shape = alpha, rate = beta)
}
plot(x, gamma[4,], type = "l", lty = 1, xlab = "x", ylab = "p")
lines(x, gamma[3, ], type = "l", lty = 2)
lines(x, gamma[2, ], type = "l", lty = 3)
lines(x, gamma[1, ], type = "l", lty = 4)
legend("topright", legend = c("n = 256", "n = 16", "n = 8", "n = 4"),
       lty = c(1, 2, 3, 4))
