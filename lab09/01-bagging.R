library(randomForest)
library(MASS)

head(Boston)
names(Boston)

medv_rf <- randomForest(medv ~ ., data = Boston, importance = TRUE)
medv_rf

plot(medv_rf, type = "l")

tail(medv_rf$mse)

plot(medv_rf$rsq, type = "l", xlab = "trees", ylab = expression(R^2))

importance(medv_rf)
varImpPlot(medv_rf)
partialPlot(medv_rf, Boston, x.var = rm)
partialPlot(medv_rf, Boston, x.var = lstat)

library(plotmo)
plotmo(medv_rf, pmethod = "partdep", degree1 = FALSE, degree2 = c("rm", "lstat"))


medv_bag <- randomForest(medv ~ ., data = Boston, mtry = 13, importance = TRUE)
medv_bag

tune_res <- tuneRF(Boston[, -14], Boston[, 14], mtryStart = 2, stepFactor = 2)


medv_rf_tuned <- NULL
row_min <- which.min(tune_res[, "OOBError"])
mtry_min <- tune_res[row_min, "mtry"]

if (!(mtry_min %in% c(medv_rf$mtry, 13))) {
  medv_rf_tuned <- randomForest(medv ~ ., data = Boston, mtry = mtry_min)
  medv_rf_tuned
} else if (mtry_min == medv_rf$mtry) {
  "Równa domyślnej"
} else {
  "Równa maksymalnej"
}

cols <- c("red", "black", "green")

plot(medv_rf, type = "l", col = cols[1], main = NULL)
lines(medv_bag$mse, col = cols[2])
lgd <- c("rf", "bag")

if (!is.null(medv_rf_tuned)) {
  lines(medv_rf_tuned$mse, col = cols[3])
  lgd <- c(lgd, "rf_tuned")
}
legend("topright", legend = lgd, col = cols, pch = 20)

set.seed(2025)
n <- nrow(Boston)
train <- sample(n, 0.8 * n)
test <- -train

medv_rf_subset <- randomForest(medv ~ ., data = Boston, subset = train)
medv_rf_subset_pred <- predict(medv_rf_subset, Boston[test,])
mean((medv_rf_subset_pred - Boston$medv[test])^2)

medv_bag_subset <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13)
medv_bag_subset_pred <- predict(medv_bag_subset, Boston[test,])
mean((medv_bag_subset_pred - Boston$medv[test])^2)

if (!is.null(medv_rf_tuned)) {
  medv_tuned_subset <- randomForest(medv ~ ., data = Boston, subset = train, mtry = mtry_min)
  medv_tuned_subset_pred <- predict(medv_tuned_subset, Boston[test,])
  mean((medv_tuned_subset_pred - Boston$medv[test])^2)
}