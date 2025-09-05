library(ggplot2)
library(MASS)      # For rlm (Robust Linear Model with IWLS)
library(gganimate)
library(dplyr)
library(tibble)

set.seed(123)

# ---- Simulate data with outliers ----
x <- 1:20
y <- 2 + 0.5 * x + rnorm(20, sd = 1)
y[c(5, 15)] <- y[c(5, 15)] + 8  # Add two high outliers

data <- tibble(x, y)

# ---- Fit OLS ----
ols_fit <- lm(y ~ x, data = data)

# ---- Define Huber Weight Function ----
huber_weights <- function(residuals, k = 1.5) {
  ifelse(abs(residuals) <= k, 1, k / abs(residuals))
}

# ---- Design Matrix X ----
X <- model.matrix(~ x, data = data)  # n x p matrix (20 x 2)

# ---- Initialize Weights ----
weights <- rep(1, nrow(X))  # start with equal weights
max_iter <- 10

results <- list()
beta_old <- c(0, 0)  # starting guess for coefficients

# ---- IWLS Iterations ----
for (i in 1:max_iter) {
  # Create diagonal weight matrix
  W <- diag(weights)  # no need for nrow/ncol when weights is a vector
  
  # Weighted least squares estimate
  beta <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)
  
  # Compute residuals
  residuals <- as.vector(y - X %*% beta)
  
  # Update weights
  weights <- huber_weights(residuals)
  
  # Save iteration results
  results[[i]] <- tibble(
    iter = i,
    x = x,
    y = y,
    weight = weights,
    intercept = beta[1],
    slope = beta[2]
  )
  
  # Convergence check
  if (all(abs(beta - beta_old) < 1e-3)) break
  beta_old <- beta
}

# Combine all iterations
results_df <- bind_rows(results)

# ---- Create OLS prediction line ----
ols_line <- tibble(
  x = seq(min(x), max(x), length.out = 100),
  y = predict(ols_fit, newdata = tibble(x = seq(min(x), max(x), length.out = 100))),
  method = "OLS"
)

# ---- Build the animation ----
p <- ggplot(results_df, aes(x, y)) +
  geom_point(aes(size = weight), color = "black") +
  geom_line(data = ols_line, aes(x, y), color = "blue", linewidth = 1.2) +
  geom_abline(aes(intercept = intercept, slope = slope),
              color = "red", linewidth = 1.2) +
  scale_size_continuous(range = c(2, 8)) +
  labs(
    title = "Iteration: {closest_state}",
    subtitle = "OLS (blue) vs IWLS (red)",
    x = "Predictor (x)",
    y = "Response (y)",
    size = "IWLS Weight"
  ) +
  theme_minimal(base_size = 14) +
  transition_states(iter, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

# ---- Render the animation ----
animate(p, nframes = length(unique(results_df$iter)) * 5, fps = 5)

# Save as GIF if desired
# anim_save("IWLS_vs_OLS.gif", animation = last_animation())
