setwd("C:/Users/Filip/Desktop/UMISI/Statistics-in-Artificial-Intelligence-and-Data-Analysis/report01")
getwd()

library(readr)
library(corrplot)
library(lmtest)

df <- read_csv("auto-mpg.csv", na = "?")
dim(df)
head(df)
summary(df)
str(df)
df$`car name` <- NULL

df$origin    <- factor(df$origin)
df$cylinders <- factor(df$cylinders)

colSums(is.na(df))

df <- na.omit(df)

boxplot(df$mpg,          main = "mpg",          col = "steelblue")
boxplot(df$displacement, main = "displacement",  col = "steelblue")
boxplot(df$horsepower,   main = "horsepower",    col = "steelblue")
boxplot(df$weight,       main = "weight",        col = "steelblue")
boxplot(df$acceleration, main = "acceleration",  col = "steelblue")

plot(df$weight, df$mpg, main = "mpg vs weight",
     xlab = "weight", ylab = "mpg", col = "steelblue", pch = 16)
abline(lm(mpg ~ weight, data = df), col = "red", lwd = 2)

plot(df$horsepower, df$mpg, main = "mpg vs horsepower",
     xlab = "horsepower", ylab = "mpg", col = "steelblue", pch = 16)
abline(lm(mpg ~ horsepower, data = df), col = "red", lwd = 2)

plot(df$displacement, df$mpg, main = "mpg vs displacement",
     xlab = "displacement", ylab = "mpg", col = "steelblue", pch = 16)
abline(lm(mpg ~ displacement, data = df), col = "red", lwd = 2)

df_num <- df[, sapply(df, is.numeric)]
cor_matrix <- cor(df_num)
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")

cor.test(df$displacement, df$weight)
cor.test(df$displacement, df$horsepower)
cor.test(df$weight, df$horsepower)
cor.test(df$weight, df$mpg)

winsorize <- function(x) {
  q      <- quantile(x, c(0.25, 0.75))
  iqr    <- q[2] - q[1]
  lower  <- q[1] - 1.5 * iqr
  upper  <- q[2] + 1.5 * iqr
  pmax(pmin(x, upper), lower)
}

df$mpg          <- winsorize(df$mpg)
df$displacement <- winsorize(df$displacement)
df$horsepower   <- winsorize(df$horsepower)
df$weight       <- winsorize(df$weight)

model_multi <- lm(mpg ~ displacement + horsepower + weight +
                    acceleration + `model year` + origin,
                  data = df)
summary(model_multi)

model_interact <- lm(mpg ~ weight * horsepower + displacement +
                       acceleration + `model year` + origin,
                     data = df)
summary(model_interact)

AIC(model_multi, model_interact)

plot(model_interact, which = 1)

dwtest(model_interact)

bptest(model_interact)

shapiro.test(residuals(model_interact))