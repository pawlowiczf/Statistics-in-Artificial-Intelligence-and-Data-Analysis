library(rpart)
library(rpart.plot)
library(ISLR)

CarseatsH <- 
  Carseats |> 
  transform(High = factor(ifelse(Sales <= 8, "No", "Yes")))
head(CarseatsH)

sales_high_rpart <- rpart(High ~ . - Sales, data = CarseatsH)
plot(sales_high_rpart, margin = 0.1, minbranch = 1.5)
text(sales_high_rpart, pretty = 0)

rpart.plot(sales_high_rpart)
rpart.rules(sales_high_rpart)
sales_high_rpart

printcp(sales_high_rpart)

# ------------------------------

set.seed(1)
n <- nrow(CarseatsH)
train <- sample(n, 0.8 * n)
test <- -train

sales_high_big <- rpart(High ~ . - Sales, data = CarseatsH, subset = train)
sales_high_big_predicted <- predict(sales_high_big, newdata = CarseatsH[test,], type = "class")
table(sales_high_big_predicted, CarseatsH$High[test])

mean(sales_high_big_predicted != CarseatsH$High[test])

rpart.plot(sales_high_big)

plotcp(sales_high_big)

n_min <- which.min(sales_high_big$cptable[, "xerror"])
cp_min <- sales_high_big$cptable[n_min, "CP"]
cp_min

sales_high_pruned_min <- prune(sales_high_big, cp = cp_min)
rpart.plot(sales_high_pruned_min)

sales_high_pruned_min_predicted <- predict(sales_high_pruned_min, newdata = CarseatsH[test,], type = "class")
table(sales_high_pruned_min_predicted, CarseatsH$High[test])

mean(sales_high_pruned_min_predicted != CarseatsH$High[test])


limit <- sum(sales_high_big$cptable[n_min, c("xerror", "xstd")])
n_1se <- min(which(sales_high_big$cptable[, "xerror"] < limit))
cp_1se <- sales_high_big$cptable[n_1se, "CP"]
cp_1se

sales_high_pruned_1se <- prune(sales_high_big, cp = cp_1se)
rpart.plot(sales_high_pruned_1se)

sales_high_pruned_1se_predicted <- predict(sales_high_pruned_1se, newdata = CarseatsH[test,], type = "class")
table(sales_high_pruned_1se_predicted, CarseatsH$High[test])

mean(sales_high_pruned_1se_predicted != CarseatsH$High[test])
