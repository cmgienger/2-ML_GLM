# ---- Packages ----
library(tidyverse)
library(patchwork)

# ---- Parameters (tweak as desired) ----
# Poisson
pois_lambda <- 3

# Negative Binomial (overdispersed counts)
# You can specify by (size, mu) so it's directly comparable to Poisson's mean
nb_size <- 1.5      # smaller = more overdispersion
nb_mu   <- 3

# Binomial (proportions)
binom_n <- 20
binom_p <- 0.6

# Bernoulli (binary)
bern_p <- 0.7

# Gamma (positive continuous)
gamma_shape <- 2
gamma_scale <- 2    # mean = shape * scale

# Gaussian (unbounded continuous)
norm_mu <- 0
norm_sd <- 1

# ---- 1) Poisson PMF (counts) ----
pois_df <- tibble(
  x = 0:max(12, qpois(0.999, pois_lambda)),
  prob = dpois(x, pois_lambda)
)

p_pois <- ggplot(pois_df, aes(x, prob)) +
  geom_col(width = 0.9, fill = "aquamarine", color = "black") +
  labs(title = "Poisson (counts)",
       subtitle = bquote(lambda == .(pois_lambda)),
       x = "Count (x)", y = "P(X = x)") +
  theme_minimal(base_size = 12)

# ---- 2) Negative Binomial PMF (overdispersed counts) ----
nb_max <- max(20, qnbinom(0.999, size = nb_size, mu = nb_mu))
nb_df <- tibble(
  x = 0:nb_max,
  prob = dnbinom(x, size = nb_size, mu = nb_mu)
)

p_nb <- ggplot(nb_df, aes(x, prob)) +
  geom_col(width = 0.9, fill = "hotpink", color = "black") +
  labs(title = "Negative Binomial (overdispersed counts)",
       subtitle = bquote(mu == .(nb_mu) ~ "," ~ size == .(nb_size)),
       x = "Count (x)", y = "P(X = x)") +
  theme_minimal(base_size = 12)

# ---- 3) Binomial for Proportions (PMF plotted on x/n) ----
k <- 0:binom_n
binom_df <- tibble(
  k = k,
  prop = k / binom_n,
  prob = dbinom(k, size = binom_n, prob = binom_p)
)

p_binom_prop <- ggplot(binom_df, aes(prop, prob)) +
  geom_col(width = 1 / binom_n * 0.9, fill = "purple", color = "black") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(title = "Binomial (proportions)",
       subtitle = bquote(n == .(binom_n) ~ "," ~ p == .(binom_p)),
       x = "Proportion of successes (k/n)", 
       y = "P(K = k)") +
  theme_minimal(base_size = 12)


# ---- 4) Bernoulli (binary response; single-trial binomial) ----
bern_df <- tibble(
  y = c(0, 1),
  prob = c(1 - bern_p, bern_p)
)

p_bern <- ggplot(bern_df, aes(x = y, y = prob, fill = factor(y))) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("0" = "sienna1", "1" = "royalblue")) +
  scale_x_continuous(breaks = c(0, 1)) +
  labs(title = "Binomial (binary; Bernoulli)",
       subtitle = bquote(p == .(bern_p)),
       x = "Outcome (0/1)", y = "P(Y = y)",
       fill = "Outcome") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")  # removes legend if you don't want it


# ---- 5) Gamma PDF (positive continuous) ----
x_gamma_max <- qgamma(0.999, shape = gamma_shape, scale = gamma_scale)
gamma_df <- tibble(
  x = seq(0, x_gamma_max, length.out = 400),
  dens = dgamma(x, shape = gamma_shape, scale = gamma_scale)
)

p_gamma <- ggplot(gamma_df, aes(x, dens)) +
  geom_line(linewidth = 1) +
  labs(title = "Gamma (positive continuous)",
       subtitle = bquote(shape == .(gamma_shape) ~ "," ~ scale == .(gamma_scale)),
       x = "x", y = "Density f(x)") +
  theme_minimal(base_size = 12)

# ---- 6) Gaussian PDF (unbounded continuous) ----
x_norm_max <- max(4 * norm_sd, qnorm(0.999, mean = norm_mu, sd = norm_sd) - norm_mu)
norm_df <- tibble(
  x = seq(norm_mu - x_norm_max, norm_mu + x_norm_max, length.out = 400),
  dens = dnorm(x, mean = norm_mu, sd = norm_sd)
)

p_norm <- ggplot(norm_df, aes(x, dens)) +
  geom_line(linewidth = 1) +
  labs(title = "Gaussian / Normal (unbounded continuous)",
       subtitle = bquote(mu == .(norm_mu) ~ "," ~ sigma == .(norm_sd)),
       x = "x", y = "Density f(x)") +
  theme_minimal(base_size = 12)

# ---- Combine 6 panels (2 x 3) ----
fig <- (p_pois | p_nb | p_binom_prop) /
  (p_bern | p_gamma | p_norm)

fig + plot_annotation(title = "Distributions used in GLM analysis",
                      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
