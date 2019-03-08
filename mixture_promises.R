
# Mixture model - Promises ------------------------------------------------
#+ setup, message = FALSE, warning = FALSE, error = FALSE

load("mixture_promise.RData")

# Packages ----------------------------------------------------------------
library(pacman)
p_load(magrittr, dplyr, forcats, tidyr, modelr, tidybayes,
       ggstance, ggridges, rstan, brms, tidyverse, bayesplot, rjags)


# Data --------------------------------------------------------------------
d <- data %>% 
  filter(dec_partner == "Trust") %>% 
  select(id, response) %>% 
  group_by(id) %>% 
  mutate(trial = 1:18) %>% 
  ungroup() %>% 
  spread(trial, response) %>% 
  select(-id) %>% 
  as.matrix()

n_subjects <- nrow(d)
n_trials <- ncol(d)
data_jags <- list(d = d, n_subjects = n_subjects, n_trials = n_trials)


# Model 1 mixture ---------------------------------------------------------

mix_1 <- "model {

# Priors

for(i in 1:n_subjects) {
  for(j in 1:n_trials) {
    theta[i, j] ~ dbeta(alpha, beta) 
  }
}

# Transformation to group mean and precision
alpha <- mu * lambda
beta <- (1 - mu) * lambda

mu ~ dunif(0, 1)
lambda ~ dunif(4, 100)

# likelihood

for(i in 1:n_subjects) {
  for(j in 1:n_trials) {
    d[i, j] ~ dbern(theta[i, j])
  }
 }
}"


# Parameters m_1 ----------------------------------------------------------
params <- c("mu", "lambda")


# Compile m_1 -------------------------------------------------------------
set.seed(721)
m_1 <- jags.model(textConnection(mix_1), 
                  data = data_jags,
                  n.chains = 3)


# Burning m_1 -------------------------------------------------------------
update(m_1, 5e3)


# Sampling from posterior m_1 ---------------------------------------------
post_m_1 <- coda.samples(m_1,
                         variable.names = params,
                         n.iter = 20e3)


# Summary m_1 -------------------------------------------------------------
summary(post_m_1)


# DIC m_1 -----------------------------------------------------------------
dic_m1 <- dic.samples(m_1, n.iter = 5e3)


# Model 2 mixtures --------------------------------------------------------

mix_2 <- "model {

# Each person belongs to one of two latent groups
for(i in 1:n_subjects) {
  z[i] ~ dbern(phi)  # phi is the base rate
  z1[i] <- z[i] + 1
}

# Uninformative prior on base rate
phi ~ dbeta(5, 5)


# likelihood

for(i in 1:n_subjects) {
  for(j in 1:n_trials) {
    d[i, j] ~ dbern(theta[i, j, z1[i]])
    theta[i, j, 1] ~ dbeta(alpha[1], beta[1])
    theta[i, j, 2] ~ dbeta(alpha[2], beta[2])
  }
}

# Transformation to group mean and precision
alpha[1] <- mu1 * lambda1
beta[1] <- (1 - mu1) * lambda1

alpha[2] <- mu2 * lambda2
beta[2] <- (1 - mu2) * lambda2

# Priors for mu1 and mu2
mu1 ~ dunif(0, 1)
lambda1 ~ dunif(4, 100)

mu2 ~ dunif(mu1, 1)
lambda2 ~ dunif(4, 100)

}"



# Parameters m_2 ----------------------------------------------------------
params2 <- c("z", "mu1", "lambda1", "mu2", "lambda2", "phi")

# Compile m_2 -------------------------------------------------------------
set.seed(1298)
m_2 <- jags.model(textConnection(mix_2),
                  data = data_jags,
                  n.chains = 3)

# Burning m_2 -------------------------------------------------------------
update(m_2, 5e3)


# Sampling from posterior m_2 ---------------------------------------------
post_m_2 <- coda.samples(m_2,
                         variable.names = params2,
                         n.iter = 20e3)


# Summary m_2 -------------------------------------------------------------
summary(post_m_2)


# DIC m_2 -----------------------------------------------------------------
dic_m2 <- dic.samples(m_2, n.iter = 5e3)


# Comparing m_1 - m_2 -----------------------------------------------------
dic_m1
dic_m2
dic_m1 - dic_m2


# Two mixtures m_2 --------------------------------------------------------

chains_m_2 <- ggmcmc::ggs(post_m_2)


x_intercepts <- chains_m_2 %>% 
  filter(Parameter %in% c("mu1", "mu2")) %>% 
  group_by(Parameter) %>% 
  summarise(estimate = mean(value)) %>% 
  pull()


(thetas <- chains_m_2 %>% 
  filter(Parameter %in% c("mu1", "mu2")) %>% 
  mutate(Parameter = factor(Parameter, 
                            levels = c("mu1", "mu2"),
                            labels = c("Dishonest", "Honest"))) %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(col = Parameter, fill = Parameter), 
                 alpha = 0.5,
                 position = "dodge") +
  geom_vline(xintercept = x_intercepts, 
             col = c("#053778", "#A4CAE3"), 
             lty = 2,
             size = 1) +
  scale_color_manual(values = c("#053778", "#A4CAE3")) +
  scale_fill_manual(values = c("#053778", "#A4CAE3")) +
  labs(x = c("Estimate" = expression(theta)), 
       y = "Count",
       col = NULL,
       fill = NULL) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_line(linetype = "blank"),
        legend.position = "bottom"))

