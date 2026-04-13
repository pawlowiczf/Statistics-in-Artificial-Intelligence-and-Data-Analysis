library(ISwR)
data("esoph")

esoph_df <- esoph

head(esoph_df)
summary(esoph_df)
sum(esoph_df$ncontrols == 0)
sum(is.na(esoph_df$ncontrols))

esoph_df$agegp <- factor(esoph_df$agegp, ordered = FALSE)
esoph_df$alcgp <- factor(esoph_df$alcgp, ordered = FALSE)
esoph_df$tobgp <- factor(esoph_df$tobgp, ordered = FALSE)

esoph_df <- subset(esoph_df, ncontrols > 0)
esoph_df <- subset(esoph_df, ncases > 0)

model_esoph <- glm(ncases ~ agegp + alcgp + tobgp + offset(log(ncontrols)), data = esoph_df, family = poisson)

summary(model_esoph)

model_esoph2 <- glm(ncases ~ alcgp + tobgp + offset(log(ncontrols)),
                    data   = esoph_df,
                    family = poisson)

summary(model_esoph2)

AIC(model_esoph, model_esoph2)

dev_res <- resid(model_esoph, type = "deviance")

summary(dev_res)

shapiro.test(dev_res)

ratio <- model_esoph$deviance / model_esoph$df.residual
cat("Overdispersion ratio:", ratio, "\n")