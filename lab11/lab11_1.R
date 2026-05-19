library(lme4)
library(ggplot2)

data(sleepstudy)

head(sleepstudy, 10)
tail(sleepstudy, 10)

sleepstudy |>
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point()

# Model liniowy najmniejszych kwadratów
ss_lm_fit <- lm(Reaction ~ Days, data = sleepstudy)
summary(ss_lm_fit)

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line(aes(y = predict(ss_lm_fit)))

sleepstudy |>
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(aes(color = Subject)) +
  geom_line(aes(y = predict(ss_lm_fit)))

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point(aes(color = Subject)) +
  geom_line(aes(y = predict(ss_lm_fit))) +
  facet_wrap(~ Subject)

# Model losowych wyrazów wolnych
ss_ri_fit <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(ss_ri_fit)

ss_ri_vcov <- as.data.frame(VarCorr(ss_ri_fit))$vcov
names(ss_ri_vcov) <- c("sigma^2_u", "sigma^2")
ss_ri_vcov

ss_ri_vcov["sigma^2_u"] / sum(ss_ri_vcov)

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_ri_fit)))

# Model losowych wyrazów wolnych i współczynników kierunkowych

ss_rsi_fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(ss_rsi_fit)

fixef(ss_rsi_fit)

ss_rsi_vc <- as.data.frame(VarCorr(ss_rsi_fit))
sigma_rho <- ss_rsi_vc$vcov
sigma_rho[3] <- ss_rsi_vc$sdcor[3]
names(sigma_rho) <- c("sigma^2_u", "sigma^2_v", "rho_uv", "sigma^2")
sigma_rho

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_rsi_fit)))

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_rsi_fit))) +
  facet_wrap(~ Subject)

# Model pośredni: ustalony współczynnik kierunkowy, losowe wyrazy wolne
ss_rifs_fit <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(ss_rifs_fit)

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_rifs_fit)))

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_rifs_fit))) +
  facet_wrap(~ Subject)

# Model pośredni: losowe współczynniki kierunkowe, ustalony wyraz wolny
ss_firs_fit <- lmer(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)
summary(ss_firs_fit)

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_firs_fit)))

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_firs_fit))) +
  facet_wrap(~ Subject)

# Model losowych wyrazów wolnych i współczynników kierunkowych z błędami niezależnymi (RSI-i)

ss_rsii_fit <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy)
summary(ss_rsii_fit)

ss_rsii_vcov <- as.data.frame(VarCorr(ss_rsii_fit))$vcov
names(ss_rsii_vcov) <- c("sigma^2_u", "sigma^2_v", "sigma^2")
ss_rsii_vcov

sleepstudy |> 
  ggplot(aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(aes(y = predict(ss_rsii_fit)))

# Porównanie modeli
anova(ss_rsi_fit, ss_rsii_fit, ss_rifs_fit, ss_firs_fit, ss_ri_fit)
coefs <- 
  list(ss_rsi_fit, ss_rsii_fit, ss_rifs_fit, ss_firs_fit) |> 
  vapply(fixef, FUN.VALUE = double(2)) |> 
  cbind(c(fixef(ss_ri_fit), NA)) |> 
  cbind(coef(ss_lm_fit))
colnames(coefs) <- c("RSI", "RSI-i", "RIFS", "FIRS", "RI", "LM")
coefs