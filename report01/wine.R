data(wine, package = "ordinal")

head(wine)
summary(wine)

model_wine <- clm(rating ~ response + temp + contact, data = wine)
summary(model_wine)

cor(as.numeric(wine$rating), wine$response)

model_wine <- clm(rating ~ temp + contact, data = wine)
summary(model_wine)


wine$rating_binary <- ifelse(as.numeric(wine$rating) < 4, 0, 1)


table(wine$rating_binary)

wine$rating_binary_factor <- factor(wine$rating_binary, ordered = TRUE)

model_clm_binary <- clm(rating_binary_factor ~ temp + contact, data = wine)
summary(model_clm_binary)

model_logit <- glm(rating_binary ~ temp + contact, data = wine, family = binomial)
summary(model_logit)

cat("CLM coefficients:\n")
coef(model_clm_binary)

cat("\nLogistic coefficients:\n")
coef(model_logit)