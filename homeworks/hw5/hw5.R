# Import histogram data
# H is a matrix with 40000 rows and 16 columns
# each row is a histogram with 16 bins
setwd("C://Users//Bangda//Desktop//GR5241//hw5")
H = matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
dim(H)
rowSums(H)[1:10]

MultinomialEM = function(H, K, tau){
  # @param H - the data matrix
  # @param K - the number of clusters
  # @param tau - threshold parameter
  # @return m - cluster label
  
  # Initilize
  H = H + .01
  set.seed(1)
  center_label = sample(1:nrow(H), K, replace = FALSE)
  t = H[center_label, ]
  # t = t / rowSums(t)
  
  # Iteration
  phi   = matrix(0, nrow = nrow(H), ncol = K)
  a     = matrix(0, nrow = nrow(H), ncol = K)
  c     = rep(1 / K, K)
  delta = Inf
  while (delta > tau){
    # E - step
    phi   = exp(H %*% t(log(t)))
    a_old = a
    a     = phi * t(replicate(nrow(H), c)) / replicate(K, as.vector(phi %*% c))
    
    # M - step
    c     = colSums(a) / nrow(H)
    b     = t(a) %*% H
    t     = b / rowSums(b)
    delta = norm(a - a_old, type = "O")
  }
  m = apply(a, MARGIN = 1, which.max)
  return(m)
}

# Run with different tau and K
tau = c(.001, .005, .01)
m1 = sapply(tau, MultinomialEM, H = H, K = 3)
m2 = sapply(tau, MultinomialEM, H = H, K = 4)
m3 = sapply(tau, MultinomialEM, H = H, K = 5)

# Convert into figures
fig1 = matrix(m1[,1], nrow = 200, ncol = 200, byrow = TRUE)
image(fig1, col = grey(seq(0, 1, length = 256)), main = "K = 3")

fig2 = matrix(m2[,1], nrow = 200, ncol = 200, byrow = TRUE)
image(fig2, col = grey(seq(0, 1, length = 256)), main = "K = 4")

fig3 = matrix(m3[,1], nrow = 200, ncol = 200, byrow = TRUE)
image(fig3, col = grey(seq(0, 1, length = 256)), main = "K = 5")
