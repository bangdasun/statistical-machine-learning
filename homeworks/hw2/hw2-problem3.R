
### Problem 3 - LDA ###
setwd("C://Users//Bangda//Desktop//GR5241//HW2")

# import the data and convert them into training and testing set
train3 = read.table("train_3.txt", sep = ",")
train5 = read.table("train_5.txt", sep = ",")
train8 = read.table("train_8.txt", sep = ",")

# Create testing set
test = read.table("zip_test.txt")
test3 = test[test[, 1] == 3, ]
test5 = test[test[, 1] == 5, ]
test8 = test[test[, 1] == 8, ]
new_test = rbind(test3, test5, test8)
colnames(new_test) <- c("label", paste("V", 1:256, sep = ""))

# Create training set
train3$label = rep(3, nrow(train3))
train5$label = rep(5, nrow(train5))
train8$label = rep(8, nrow(train8))
new_train = rbind(train3, train5, train8)

# Build LDA directly
library(MASS)
lda_model = lda(label ~., data = new_train, na.action = na.omit)

# Visualize the classification
plot(lda_model)

# Create the confusion matrix of training set
lda_model_pred_train = predict(lda_model, new_train)
(confusion1_train = table(new_train$label, lda_model_pred_train$class))

# Training error
(train_err1 = 1 - sum(diag(confusion1_train)) / nrow(new_train))

# Create the confusion matrix of testing set
lda_model_pred_test = predict(lda_model, new_test)
(confusion1_test = table(new_test$label, lda_model_pred_test$class))

# Testing error
(test_err1 = 1 - sum(diag(confusion1_test)) / nrow(new_test))

### Problem 3 - LDA with PCA ###
# Use PCA
pr = prcomp(new_train[, -257], center = TRUE, scale. = TRUE)
score = pr$x[, 1:49]

# Create new training set
new_train2 = cbind(new_train$label, score)
new_train2 = as.data.frame(new_train2)
colnames(new_train2) = c("label", paste("PC", 1:49, sep = ""))
lda_model2 = lda(label ~., data = new_train2, na.action = na.omit)

# Visualize the classification
plot(lda_model2)

# Calculate the confusion matrix and error on the training set
lda_model2_pred_train = predict(lda_model2, new_train2)
(confusion2_train = table(new_train2$label, lda_model2_pred_train$class))
(train_err2 = 1 - sum(diag(confusion2_train)) / nrow(new_train2))

# Calculate the confusion matrix and error on the testing set
new_test2 = as.matrix(scale(new_test[, -1], center = TRUE, scale = TRUE)) %*% pr$rotation[, 1:49]
new_test2 = as.data.frame(cbind(new_test$label, new_test2[,1:49]))
colnames(new_test2) = c("label", paste("PC", 1:49, sep = ""))
lda_model2_pred_test = predict(lda_model2, new_test2)
(confusion2_test = table(new_test2$label, lda_model2_pred_test$class))
(test_err2 = 1 - sum(diag(confusion2_test)) / nrow(new_test2))

### Problem 3 - LDA with filtered data ### 
# Use average to replace the original value
average_2by2 = function(vec){
  topleft = matrix(1:256, ncol = 16)
  topleft = topleft[seq(1, nrow(topleft), by = 2), seq(1, ncol(topleft), by = 2)]
  topleft = as.vector(topleft)
  for (i in topleft){
    ave = sum(vec[i] + vec[i + 1] + vec[i + 16] + vec[i + 17]) / 4
    vec[i] = vec[i + 1] = vec[i + 16] = vec[i + 17] = ave
  }
  return(vec)
}
new_train3 = apply(new_train[, -257], MARGIN = 1, average_2by2)
new_train3 = cbind(new_train$label, t(new_train3))
new_train3 = as.data.frame(new_train3)
colnames(new_train3) = c("label", paste("V", 1:256, sep = ""))

# LDA
lda_model3 = lda(label ~., data = new_train3, na.action = na.omit)
plot(lda_model3)

# Calculate the confusion matrix and error on training set
lda_model3_pred_train = predict(lda_model3, new_train3)
(confusion3_train = table(new_train3$label, lda_model3_pred_train$class))
(train_err3 = 1 - sum(diag(confusion3_train)) / nrow(new_train3))

# Calculate the confustion matrix and error on testing set
new_test3 = apply(new_test[, -1], MARGIN = 1, average_2by2)
new_test3 = cbind(new_test$label, t(new_test3))
new_test3 = as.data.frame(new_test3)
colnames(new_test3) = c("label", paste("V", 1:256, sep = ""))
lda_model3_pred_test = predict(lda_model3, new_test3)
(confusion3_test = table(new_test3$label, lda_model3_pred_test$class))
(test_err3 = 1 - sum(diag(confusion3_test)) / nrow(new_test3))

### Problem 3 - Logistic Regression with multinomial family ### 
library(glmnet)
x = as.matrix(new_train3[, -1])
y = as.matrix(new_train3[, 1])
lr_model = glmnet(x = x, y = y, family = "multinomial")
# Prediction on training set
lr_model_pred_train = predict(lr_model, newx = x, type = "class", s = .01)
(confusion4_train = table(new_train3$label, lr_model_pred_train))
(train_err4 = 1 - sum(diag(confusion4_train)) / nrow(x))
# Prediction on testing set
lr_model_pred_test = predict(lr_model, newx = as.matrix(new_test3[, -1]),
                             type = "class", s = .01)
(confusion4_test = table(new_test3$label, lr_model_pred_test))
(test_err4 = 1 - sum(diag(confusion4_test)) / nrow(new_test3))

### Result
train_err = c(train_err1, train_err2, train_err3, train_err4)
test_err = c(test_err1, test_err2, test_err3, test_err4)
plot(test_err, type = "b", col = "red", pch = 1, xlim = c(1,4), 
     ylim = c(0, max(test_err) + .05), axes = FALSE, ylab = "Rate", xlab = "")
axis(side = 2, at = seq(0, max(test_err) + .05, .05))
axis(side = 1, at = seq(1, 4), 
     labels = c("LDA", "LDA with PCA", "LDA with filtered data", "Logistic Reg"))
lines(train_err, type = "b", col = "blue", pch = 2)
legend("topright", legend = c("Test Error", "Train Error"), 
       pch = c(1, 2), col = c("red", "blue"))
