#
# caret overview: classification and regression training
#
# streamling the model building and evaluation process
#
# primary function: train() 
# (while it also has function with name to be the name of specific algorithms)
#
# for each parameter set:
#   for each resampling iteration:
#     hold-out specific samples
#     fit the model on the remainder
#     predict the hold-out samples
#   calculate average performance across hold-out predictions
# determine the optimal parameter set
# fit the final model to all the training data using the optimal parameter set

# ==================================== example ===========================================
# get train/test --> training model --> tuning (cv and resampling) --> predict
library(caret)
library(mlbench)
library(tidyverse)
data("Sonar")
glimpse(Sonar) # 208 x 60


train_idx <- createDataPartition(y = Sonar$Class, p = .8, list = FALSE)
str(train_idx)

train <- Sonar[train_idx, ]
test  <- Sonar[-train_idx, ]

# use partial least discriminant analysis (PLSDA)
pls_model <- train(Class ~., data = train, method = 'pls', preProc = c('center', 'scale'))

# extend this training process - to be more accurate
# * expanding function evaluates
# * resampling method and folds set of cv
# * measuring performance

# 1. tuneLength = ..., tuneGrid = ... for candidate parameters. tuneGrid should be data frame
pls_model <- train(Class ~., data = train, method = 'pls', tuneLength = 15, 
  preProc = c('center', 'scale'))

# 2. trainControl() for resampling and cv, method = 'boot'/'repeatedcv'
ctrl <- trainControl(method = 'repeatedcv', repeats = 5)
pls_model <- train(Class ~., data = train, method = 'pls', tuneLength = 15,
  trControl = ctrl, preProc = c('center', 'scale'))

# 3. trainControl()/train() for measuring performance
set.seed(2333)
ctrl <- trainControl(method = 'repeatedcv', repeats = 5, classProbs = TRUE, 
  summaryFunction = twoClassSummary)
pls_model <- train(Class ~., data = train, method = 'pls', tuneLength = 15,
  trControl = ctrl, metric = 'ROC', preProc = c('center', 'scale'))

pls_model
plot(pls_model)

# prediction and confusion matrix
pls_predict <- predict(pls_model, newdata = test)
pls_predict_prob <- predict(pls_model, newdata = test, type = 'prob')
confusionMatrix(data = pls_predict, test$Class)

# =============================== end of example ==========================================

# more references:
#   ?resamples
#   https://www.medcalc.org/manual/roc-curves.php
#   http://topepo.github.io/caret/
