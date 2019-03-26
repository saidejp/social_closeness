
# Monotonic predictors ----------------------------------------------------

# Packages ----------------------------------------------------------------

library(pacman)
p_load(magrittr, dplyr, forcats, tidyr, modelr, tidybayes,
       ggstance, ggridges, rstan, brms, tidyverse, bayesplot)


# Reading data ------------------------------------------------------------

data <- read.csv("data.csv") %>% as_tibble()



# Creating ordinal predictors ---------------------------------------------

d_ord <- data %>% 
  mutate(
    prom_ord = case_when(
      prom_keys == "No response" ~ 0,
      prom_keys == "Never" ~ 1,
      prom_keys == "Sometimes" ~ 2,
      prom_keys == "Mostly" ~ 3,
      prom_keys == "Always" ~ 4,
      is.na(prom_keys) & promise == "No promise" ~ 0
    ),
    part_ord = case_when(
      partner == "Computer" ~ 0,
      partner == "Confederate" ~ 1,
      partner == "Friend" ~ 2
    )
  ) %>% 
  fill(prom_ord) %>% 
  filter(dec_partner == "Trust") %>% 
  mutate(not_pay = if_else(response == 1, 0, 1)) %>% 
  select(id, part_ord, prom_ord, not_pay)



# Modeling ----------------------------------------------------------------



# M0 varying intercepts ---------------------------------------------------

m0 <- brm(not_pay ~ 1 + (1 | id),
          data = d_ord,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))

m0 <- add_criterion(m0, "kfold", K = 10)


# M1 ordinal predictor prom -----------------------------------------------

prior1 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1")

m1 <- brm(not_pay ~ mo(prom_ord) + (mo(prom_ord) | id),
          data = d_ord,
          prior = prior1,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))

m1 <- add_criterion(m1, "kfold", K = 10)


# M2 ordinal predictor partner --------------------------------------------

prior2 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1")


m2 <- brm(not_pay ~ mo(part_ord) + (mo(part_ord) | id),
          data = d_ord,
          prior = prior2,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))

m2 <- add_criterion(m2, "kfold", K = 10)


# M3 ordinal predictos prom + partner -------------------------------------

prior3 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1")

m3 <- brm(not_pay ~ mo(prom_ord) + mo(part_ord) + (mo(prom_ord) + mo(part_ord) | id),
          data = d_ord,
          prior = prior3,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))

m3 <- add_criterion(m3, "kfold", K = 10)


# M4 ordinal predictos prom * partner -------------------------------------

m4 <- brm(not_pay ~ mo(prom_ord) *  mo(part_ord) + (mo(prom_ord) *  mo(part_ord) | id),
          data = d_ord,
          prior = prior3,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))


m4 <- add_criterion(m4, "kfold", K = 10)


# Comparing models --------------------------------------------------------

loo_compare(m0, m1, m2, m3, m4, criterion =  "kfold")

