library(ISLR)
library(leaps)

Hitters <- na.omit(Hitters)
dim(Hitters)

Hitters_bs <- regsubsets(Salary ~ ., data = Hitters)
summary(Hitters_bs)

Hitters_bs <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
Hitters_bs_sum <- summary(Hitters_bs)
Hitters_bs_sum$cp

bic_min <- which.min(Hitters_bs_sum$bic)
bic_min
Hitters_bs_sum$bic[bic_min]

plot(Hitters_bs_sum$bic, xlab = "Liczba zmiennych", ylab = "BIC", col = "green",
     type = "b", pch = 20)
points(bic_min, Hitters_bs_sum$bic[bic_min], col = "red", pch = 9)

plot(Hitters_bs, scale = "bic")
coef(Hitters_bs, id = 6)

# -----------------

Hitters_fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, 
                          method = "forward")
Hitters_fwd_sum <- summary(Hitters_fwd)
Hitters_fwd_sum
Hitters_back <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, 
                           method = "backward")
Hitters_back_sum <- summary(Hitters_back)
Hitters_back_sum

n <- nrow(Hitters)
train <- sample(c(TRUE, FALSE), n, replace = TRUE)
test <- !train
Hitters_bs_v <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19)

predict.regsubsets <- function(object, newdata, id, ...) {
  model_formula <- as.formula(object$call[[2]])
  mat <- model.matrix(model_formula, newdata)
  coefs <- coef(object, id = id)
  mat[, names(coefs)] %*% coefs
}

prediction_error <- function(i, model, subset) {
  pred <- predict(model, Hitters[subset,], id = i)
  mean((Hitters$Salary[subset] - pred)^2)
}
val_errors <- sapply(1:19, prediction_error, model = Hitters_bs_v, subset = test)
val_errors

k <- 10
folds <- sample(1:k, n, replace = TRUE)
val_err <- NULL
for (j in 1:k) {
  fit_bs <- regsubsets(Salary ~ ., data = Hitters[folds != j,], nvmax = 19)
  err <- sapply(1:19, prediction_error, model = fit_bs, subset = (folds == j))
  val_err <- rbind(val_err, err)
}

cv_errors <- colMeans(val_err)
cv_errors