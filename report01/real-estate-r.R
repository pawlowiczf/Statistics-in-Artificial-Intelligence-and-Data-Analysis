setwd("C:/Users/Filip/Desktop/UMISI/Statistics-in-Artificial-Intelligence-and-Data-Analysis/report01")
getwd()

library(readxl)

df <- read_excel("real-estate.xlsx")
dim(df)

head(df)
summary(df)

df$No <- NULL
names(df) <- c("date", "age", "dist_mrt", "n_stores", "lat", "lon", "price")

head(df)

colSums(is.na(df))

boxplot(df$price, main = "price", col = "steelblue")
boxplot(df$dist_mrt, main = "dist_mrt", col = "steelblue")
boxplot(df$age, main = "age", col = "steelblue")
boxplot(df$n_stores, main = "age", col = "steelblue")

plot(df$dist_mrt, df$price, main = "price vs dist_mrt",
     xlab = "dist_mrt", ylab = "price", col = "steelblue", pch = 16)
abline(lm(price ~ dist_mrt, data = df), col = "red", lwd = 2)

plot(df$age, df$price, main = "price vs age",
     xlab = "age", ylab = "price", col = "steelblue", pch = 16)
abline(lm(price ~ age, data = df), col = "red", lwd = 2)

cor_matrix <- cor(df)
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")

plot(df$lon, df$dist_mrt, main = "longitude vs dist_mrt",
     xlab = "longitude", ylab = "dist_mrt", col = "steelblue", pch = 16)
abline(lm(dist_mrt ~ lon, data = df), col = "red", lwd = 2)

cor.test(df$dist_mrt, df$price)
cor.test(df$n_stores, df$price)
cor.test(df$lat, df$lon)
cor.test(df$dist_mrt, df$lon)

df$lon <- NULL

model_tmp <- lm(price ~ ., data = df)

# Policz Cooks Distance
cooksd <- cooks.distance(model_tmp)

# Narysuj
plot(cooksd, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's D",
     xlab = "Nr obserwacji")

# Czerwona linia = prog (4/n to popularna regula)
abline(h = 4 / nrow(df), col = "red", lty = 2)

# Function that clips values to 1.5 * IQR boundary
winsorize <- function(x) {
  q      <- quantile(x, c(0.25, 0.75))
  iqr    <- q[2] - q[1]
  lower  <- q[1] - 1.5 * iqr
  upper  <- q[2] + 1.5 * iqr
  pmax(pmin(x, upper), lower)   # clip from both sides
}

# Apply to each numeric column
df$price    <- winsorize(df$price)
df$dist_mrt <- winsorize(df$dist_mrt)
df$age      <- winsorize(df$age)
names(df)

model_multi <- lm(price ~ ., data = df)
summary(model_multi)

model_interact <- lm(price ~ dist_mrt * n_stores + dist_mrt * lat + age + date, data = df)
summary(model_interact)

AIC(model_multi, model_interact)

plot(model_interact, which = 1)

library(lmtest)
dwtest(model_interact)

bptest(model_interact)

shapiro.test(residuals(model_interact))
