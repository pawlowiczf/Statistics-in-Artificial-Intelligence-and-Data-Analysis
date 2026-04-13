setwd("C:/Users/Filip/Desktop/UMISI/Statistics-in-Artificial-Intelligence-and-Data-Analysis/report01")
getwd()

df <- read.table("poisson.data", header = TRUE, sep = ";", dec = ",")

head(df)
summary(df)
dim(df)


likelihood <- function(params, y, x) {
  a0     <- params[1]
  a1     <- params[2]
  eta    <- a0 + a1 * x
  lambda <- exp(eta)
  nll    <- -sum(y * eta - lambda)
  return(nll)
}

names(df)
y <- df$Y
x <- df$X1

result <- optim(par = c(0, 0), fn = likelihood, y = y, x = x)

a0_optim <- result$par[1]
a1_optim <- result$par[2]

cat("a0 (intercept):", a0_optim, "\n")
cat("a1 (slope)    :", a1_optim, "\n")

model_poisson <- glm(y ~ x, data = df, family = poisson)
summary(model_poisson)

?cat
cat("optim: a0 =", a0_optim, "  a1 =", a1_optim, "\n")
cat("glm:   a0 =", coef(model_poisson)[1], "  a1 =", coef(model_poisson)[2], "\n")

dev_res <- residuals(model_poisson, type = "deviance")

# Basic statistics
summary(dev_res)
