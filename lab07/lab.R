library('ISLR')

fit_poly <- lm(wage ~ poly(age, 4), data = Wage)
summary(fit_poly)

fit_poly_raw <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
summary(fit_poly_raw)

age_lims <- range(Wage$age)
age_grid <- seq(age_lims[1], age_lims[2])
pred_poly <- predict(fit_poly, list(age = age_grid), se.fit = TRUE)
se_bands <- cbind(pred_poly$fit + 2 * pred_poly$se.fit,
                  pred_poly$fit - 2 * pred_poly$se.fit)
plot(Wage$age, Wage$wage, col = "darkgrey", cex = 0.5, xlim = age_lims)
lines(age_grid, pred_poly$fit, col = "red", lwd = 2)
matlines(age_grid, se_bands, col = "red", lty = "dashed")

fit_log_poly <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

pred_log_poly <- predict(fit_log_poly, list(age = age_grid), se.fit = TRUE)
pred_probs <- plogis(pred_log_poly$fit)
se_bands_logit <- cbind(pred_log_poly$fit + 2 * pred_log_poly$se.fit,
                        pred_log_poly$fit - 2 * pred_log_poly$se.fit)
se_bands <- plogis(se_bands_logit)
plot(Wage$age, I(Wage$wage > 250), xlim = age_lims, ylim = c(0, 0.2),
     col = "darkgrey", cex = 0.5, ylab = "P(wage > 250 | age)")
lines(age_grid, pred_probs, col = "red", lwd = 2)
matlines(age_grid, se_bands, lty = "dashed", col = "red")

#####

table(cut(Wage$age, breaks = 4))

fit_step <- lm(wage ~ cut(age, 4), data = Wage)
pred_step <- predict(fit_step, list(age = age_grid), se.fit = TRUE)
se_bands <- cbind(pred_step$fit + 2 * pred_step$se.fit,
                  pred_step$fit - 2 * pred_step$se.fit)
plot(Wage$age, Wage$wage, col = "darkgrey", cex = 0.5, xlim = age_lims)
lines(age_grid, pred_step$fit, col = "red", lwd = 2)
matlines(age_grid, se_bands, col = "red", lty = "dashed")

#####
library('splines')

fit_bs_knots <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred_bs_knots <- predict(fit_bs_knots, list(age = age_grid), se.fit = TRUE)
plot(Wage$age, Wage$wage, cex = 0.5, col = "darkgrey")
lines(age_grid, pred_bs_knots$fit, col = "red", lwd = 2)
lines(age_grid, pred_bs_knots$fit + 2 * pred_bs_knots$se.fit, col = "red",
      lty = "dashed")
lines(age_grid, pred_bs_knots$fit - 2 * pred_bs_knots$se.fit, col = "red",
      lty = "dashed")
abline(v = c(25, 40, 60), lty = "dotted")

fit_bs_df <- lm(wage ~ bs(age, df = 6), data = Wage)
pred_bs_df <- predict(fit_bs_df, list(age = age_grid), se.fit = TRUE)
plot(Wage$age, Wage$wage, cex = 0.5, col = "darkgrey")
lines(age_grid, pred_bs_df$fit, col = "red", lwd = 2)
lines(age_grid, pred_bs_df$fit + 2 * pred_bs_df$se.fit, col = "red",
      lty = "dashed")
lines(age_grid, pred_bs_df$fit - 2 * pred_bs_df$se.fit, col = "red",
      lty = "dashed")
bs_knots <- attr(bs(Wage$age, df = 6), "knots")
abline(v = bs_knots, lty = "dotted")
