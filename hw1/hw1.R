#################
### Problem 2 ###
#################

### Question 1 ### 
# install.packages("leaps")
library(leaps)

# set work directory
setwd("C://Users//Bangda//Desktop//GR5241//HW1")

# load the data
Credit <- read.csv("Credit.csv", header = TRUE)
head(Credit)
Credit <- Credit[ , -1]

# Build model
model1 <- regsubsets(Balance ~., data = Credit, nvmax = 11)
model2 <- regsubsets(Balance ~., data = Credit, method = "forward", nvmax = 11)
model3 <- regsubsets(Balance ~., data = Credit, method = "backward", nvmax = 11)

# Get the RSS
rss1 <- summary(model1)$rss
rss2 <- summary(model2)$rss
rss3 <- summary(model3)$rss

# Plotting RSS
plot(rss1, type = "b", col = "red", pch = 1, xlab = "Number of variables",
     ylab = "RSS")
lines(rss2, type = "b", col = "black", pch = 2)
lines(rss3, type = "b", col = "blue", pch = 3)
legend("topright", legend = c("Best Subseclection", "Forward stepwise", "Backward stepwis"),
       pch = c(1, 2, 3), col = c("red", "black", "blue"), lty = c(1, 1, 1))

### Question 2 ###
# Get the Cp and BIC of models
Cp1 <- summary(model1)$cp
Cp2 <- summary(model2)$cp
Cp3 <- summary(model3)$cp
BIC1 <- summary(model1)$bic
BIC2 <- summary(model2)$bic
BIC3 <- summary(model3)$bic

# Select the optimal model
# Based on Cp
Cp <- matrix(c(Cp1, Cp2, Cp3), ncol = 3)
apply(Cp, 2, which.min)
# Based on BIC
BIC <- matrix(c(BIC1, BIC2, BIC3), ncol = 3)
apply(BIC, 2, which.min)

summary(model1)$which[c(4, 6),]
summary(model2)$which[c(5, 6),]
summary(model3)$which[c(4, 6),]

#################
### Problem 3 ###
#################

### Question 1 ###
# Stock code
stock_code <- c("UNH", "JNJ", "TRV", "UTX", "WMT",
                "AAPL", "CVX", "DD", "MCD", "KO",
                "DIS", "CSCO", "NKE", "MMM", "PFE",
                "VZ", "MRK", "IBM", "PG", "HD",
                "MSFT", "GE", "CAT", "BA", "V",
                "AXP", "XOM", "INTC", "JPM", "GS")

# Structure of the file url
url1 <- "http://ichart.finance.yahoo.com/table.csv?s="
url2 <- "&a=00&b=1&c=2010&d=00&e=1&f=2011&g=d&ignore=.csv"

# Generate file url to download
fileurl <- paste(url1, stock_code, url2, sep = "")

# Generate file name
destfile <- paste("C://Users//Bangda//Desktop//GR5241//HW1//", stock_code, ".csv", sep = "")

# Download data
for (i in 1:length(destfile)){
  download.file(fileurl[i], destfile = destfile[i])
}

# Load the data and get the close price
filename <- paste(stock_code, ".csv", sep = "")
close_price <- c()
for (i in 1:length(filename)){
  close_price <- cbind(close_price, read.csv(filename[i], header = TRUE)[, "Close"])
}
colnames(close_price) <- stock_code

### Question 2 ###
# Perform PCA on close_price with cor = FALSE
pr_out1 <- princomp(close_price, cor = FALSE)
biplot(pr_out1)

# Compute the PVE and cumulative PVE
pr_var1 <- (pr_out1$sdev)^2
pr_pve1 <- pr_var1 / sum(pr_var1)
pr_cpve1 <- cumsum(pr_pve1)
par(mfrow = c(1, 2))
plot(pr_pve1, xlab = "Principal Components", 
     ylab = "PVE", ylim = c(0, 1))
lines(pr_pve1, type = "l")
plot(pr_cpve1, xlab = "Principal Components", 
     ylab = "Cumulative PVE", ylim = c(0, 1))
lines(pr_cpve1, type = "l")

### Question 3 ###
# Perform PCA on close_price with cor = TRUE
pr_out2 <- princomp(close_price, cor = TRUE)
biplot(pr_out2)

# Compute the PVE and cumulative PVE
pr_var2 <- (pr_out2$sdev)^2
pr_pve2 <- pr_var2 / sum(pr_var2)
pr_cpve2 <- cumsum(pr_pve2)
par(mfrow = c(1, 2))
plot(pr_pve2, xlab = "Principal Components", 
     ylab = "PVE", ylim = c(0, 1))
lines(pr_pve2, type = "l")
plot(pr_cpve2, xlab = "Principal Components", 
     ylab = "Cumulative PVE", ylim = c(0, 1))
lines(pr_cpve2, type = "l")

### Question 4 ###
# Calculate the return ove every stock
price_return <- apply(close_price, 2, function(data){
  data <- log(data)
  re <- exp(diff(rev(data))) - 1
  return(rev(re))})

# Perform PCA on close_price with cor = TRUE
pr_out3 <- princomp(price_return, cor = TRUE)
biplot(pr_out3)

# Compute the PVE and cumulative PVE
pr_var3 <- (pr_out3$sdev)^2
pr_pve3 <- pr_var3 / sum(pr_var3)
pr_cpve3 <- cumsum(pr_pve3)
par(mfrow = c(1, 2))
plot(pr_pve3, xlab = "Principal Components", 
     ylab = "PVE", ylim = c(0, 1))
lines(pr_pve3, type = "l")
plot(pr_cpve3, xlab = "Principal Components", 
     ylab = "Cumulative PVE", ylim = c(0, 1))
lines(pr_cpve3, type = "l")
