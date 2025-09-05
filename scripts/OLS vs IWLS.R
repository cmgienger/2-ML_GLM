# Load packages
library(ggplot2)
library(MASS)   # for rlm (robust regression using IWLS)
set.seed(123)

# Simulate data with some outliers
x <- 1:20
y <- 2 + 0.5*x + rnorm(20, sd=1)
y[c(5, 15)] <- y[c(5, 15)] + 8   # add outliers

data <- data.frame(x, y)

# OLS fit
ols_fit <- lm(y ~ x, data = data)

# IWLS fit using robust regression
iwls_fit <- rlm(y ~ x, data = data, psi = psi.huber) # Huber weights

# Extract weights from IWLS
weights_df <- data.frame(x = x, 
                         y = y, 
                         weights = iwls_fit$w)

# Plot OLS vs IWLS
p1 <- ggplot(data, aes(x, y)) +
  geom_point(aes(size = weights_df$weights), color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  geom_abline(intercept = coef(iwls_fit)[1], slope = coef(iwls_fit)[2], color = "red") +
  labs(title = "OLS (Blue) vs IWLS (Red)",
       subtitle = "Point size = IWLS weight",
       y = "Response", x = "Predictor") +
  theme_minimal()

p1
