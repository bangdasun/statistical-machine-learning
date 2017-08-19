# random forest 
#
# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# parameters to be tuned:
# 1. mtry: number of variable to be sampled at each split
# 2. ntree: number of trees


# data
set.seed(2333)
library(mlbench)
data(Sonar)
X <- Sonar[, 1:60]
y <- Sonar[, 61]

# packages
library(caret)
library(randomForest)

# default case - naive and simple and needed parameter tuned
rf_ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
tune_grid <- expand.grid(mtry = sqrt(ncol(X)))

rf_model_default <- train(Class ~., data = Sonar, method = 'rf',
  metric = 'Accuracy', tuneGrid = tune_grid)

print(rf_model_default)

# ======================== tune using caret ==============================================
# random search
ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'random')
mtry <- sqrt(ncol(X))
rf_model_random <- train(Class ~., data = Sonar, method = 'rf',
  metric = 'Accuracy', tuneLength = 15, trControl = ctrl)
plot(rf_model_random)

# grid search
tune_grid <- expand.grid(.mtry = 1:15)
rf_model_gridsearch <- train(Class ~., data = Sonar, method = 'rf',
  metric = 'Accuracy', tuneGrid = tune_grid, trControl = ctrl)
plot(rf_model_gridsearch)

# ======================== tune using algorithm tools ====================================
# this is only used for specific algorithms (rf)
best_mtry <- randomForest::tuneRF(X, y, stepFactor = 1.5, improve = 1e-5, ntree = 500)

# ======================== customized tuning =============================================
# tune manually
ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')
tune_grid <- expand.grid(.mtry = c(sqrt(ncol(X))))
model_list <- list()
for (ntree in seq(1000, 2500, 500)) {
  fit <- train(Class ~., data = Sonar, method = 'rf',
    metric = 'Accuracy', tuneGrid = tune_grid, trControl = ctrl, ntree = ntree)
  key <- as.character(ntree)
  model_list[[key]] <- fit
}
resuls <- resamples(model_list)

# extend caret
# we need define methods like in OOP
custom_rf <- list(type = 'Classification', library = 'randomForest', loop = NULL)
custom_rf$parameters <- data.frame(parameter = c('mtry', 'ntree'),
  class = rep('numeric', 2), label = c('mtry', 'ntree'))
custom_rf$grid <- function(X, y, len = NULL, search = 'grid') {}
custom_rf$fit  <- function(X, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(X, y, mtry = param$mtry, ntree = param$ntree, ...)
}
custom_rf$predict <- function(model_fit, newdata, preProc = NULL, submodels = NULL) {
  predict(model_fit, newdata)
}
custom_rf$prob <- function(model_fit, newdata, preProc = NULL, submodels = NULL) {
  predict(model_fit, newdata, type = 'prob')
}
custom_rf$sort <- function(x) x[order(x[, 1]), ]
custom_rf$levels <- function(x) x$classes

ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
tune_grid <- expand.grid(.mtry = 1:15, .ntree = seq(1000, 2500, 500))
rf_custom <- train(Class ~., data = Sonar, method = custom_rf,
  metric = 'Accuracy', tuneGrid = tune_grid, trControl = ctrl)