library(MASS)
data("ships")

head(ships)
summary(ships)
str(ships)

names(ships)

ships$type   <- factor(ships$type)
ships$year   <- factor(ships$year)
ships$period <- factor(ships$period)

ships_df <- subset(ships, service > 0)

model_ships <- glm(incidents ~ type + year + period + offset(log(service)), data = ships_df, family = poisson)
summary(model_ships)

dev_res <- resid(model_ships, type = "deviance")

hist(dev_res, breaks = 20, col = "steelblue",
     main = "Deviance Residuals - ships",
     xlab = "Value")
abline(v = 0, col = "red", lwd = 2)

# Q-Q plot
qqnorm(dev_res, main = "Q-Q plot reszt")
qqline(dev_res, col = "red", lwd = 2)

# Shapiro-Wilk test
shapiro.test(dev_res)
# p > 0.05 -> reszty normalne
# p < 0.05 -> reszty nie sa normalne