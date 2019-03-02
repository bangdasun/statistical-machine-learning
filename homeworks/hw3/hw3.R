setwd("C:\\Users\\Bangda\\Desktop\\GR5241\\HW3")
# Import data
data5 = read.table("train5.txt", sep = ",")
data6 = read.table("train6.txt", sep = ",")
data5$label = -1
data6$label = 1
data = rbind(data5, data6)

# Split into training and testing set
num_test = nrow(data)*.2
set.seed(1234)
label_test = sample(1:nrow(data), num_test, replace = FALSE)
test = data[label_test, ]
train = data[-label_test, ]

# Build SVM classifiers
library(e1071)
svm_linear = tune.svm(label ~., data = train, kernel = "linear",
                      cost = seq(.05, .95, by = .05),
                      scale = FALSE)
summaries = summary(svm_linear)
names(summaries)
cost = summaries$performances[,1]
error = summaries$performances[, 2]
plot(cost, error, type = "l", xlab = "Cost (Margin) parameter",
     ylab = "Misclassification rate")


svm_linear2 = tune.svm(label ~., data = train, kernel = "linear",
                       cost = seq(.001, .006, by = .0001),
                       scale = FALSE)
summaries2 = summary(svm_linear2)
cost2 = summaries2$performances[,1]
error2 = summaries2$performances[, 2]
plot(cost2, error2, type = "l", xlab = "Cost (Margin) parameter",
     ylab = "Misclassification rate")
cost2[which.min(error2)]


svm_rbf = tune.svm(label ~., data = train, kernel = "radial",
                   cost = seq(1, 5, by = .1),
                   gamma = seq(.005, .02, by = .001),
                   scale = FALSE)
summaries3 = summary(svm_rbf)
plot(svm_rbf, main = "")

# Extract model
optim_linear = summaries2$best.model
optim_rbf = summaries3$best.model

# Predict linear svm
pred_linear = predict(optim_linear, test)
pred_linear = ifelse(pred_linear > 0, 1, -1)
(pred_linear_err = sum((pred_linear != test$label)) / length(pred_linear))

# Predict RBF svm
pred_rbf = predict(optim_rbf, test)
pred_rbf = ifelse(pred_rbf > 0, 1, -1)
(pred_rbf_err = sum((pred_rbf != test$label)) / length(pred_rbf))
