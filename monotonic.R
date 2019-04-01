
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

m0 <- add_criterion(m0, criterion = "waic")


# M1 ordinal predictor prom -----------------------------------------------

prior1 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1")

m1 <- brm(not_pay ~ mo(prom_ord) + (mo(prom_ord) | id),
          data = d_ord,
          prior = prior1,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))

m1 <- add_criterion(m1, criterion = "waic")


# M2 ordinal predictor partner --------------------------------------------

prior2 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1")


m2 <- brm(not_pay ~ mo(part_ord) + (mo(part_ord) | id),
          data = d_ord,
          prior = prior2,
          sample_prior = "yes",
          family = bernoulli("logit"),
          control = list(adapt_delta = 0.95))

m2 <- add_criterion(m2, criterion = "waic")


# M3 ordinal predictors prom + partner -------------------------------------

prior3 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1")

m3 <- brm(
  not_pay ~ mo(prom_ord) + mo(part_ord) + (mo(prom_ord) + mo(part_ord) | id),
  data = d_ord,
  prior = prior3,
  sample_prior = "yes",
  family = bernoulli("logit"),
  control = list(adapt_delta = 0.95)
  )

m3 <- add_criterion(m3, criterion = "waic")


# M4 ordinal predictors mo(prom) * partner ---------------------------------

prior4 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord:part_ord1")

m4 <- brm(
  not_pay ~ mo(prom_ord) * part_ord + (mo(prom_ord) * part_ord | id),
  data = d_ord,
  prior = prior4,
  sample_prior = "yes",
  family = bernoulli("logit"),
  control = list(adapt_delta = 0.95)
  )

m4 <- add_criterion(m4, criterion = "waic")


# M5 ordinal predictors prom * mo(partner) ---------------------------------

prior5 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord:prom_ord1")

m5 <- brm(
  not_pay ~ prom_ord * mo(part_ord) + (prom_ord * mo(part_ord) | id),
  data = d_ord,
  prior = prior5,
  sample_prior = "yes",
  family = bernoulli("logit"),
  control = list(adapt_delta = 0.99)
)

m5 <- add_criterion(m5, criterion = "waic")

# M6 ordinal predictors mo(prom) * mo(part) ---------------------------------

prior6 <- prior(normal(0, 10), class = "b") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord:mopart_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "moprom_ord:mopart_ord2")

m6 <- brm(
  not_pay ~ mo(prom_ord) * mo(part_ord) + (mo(prom_ord) *  mo(part_ord) | id),
  data = d_ord,
  prior = prior6,
  sample_prior = "yes",
  family = bernoulli("logit"),
  control = list(adapt_delta = 0.99)
)

m6 <- add_criterion(m6, criterion = "waic")

# m7

m7 <- brm(
  not_pay ~ mo(prom_ord) + (mo(prom_ord)  | id),
  data = d_ord,
  #prior = prior4,
  sample_prior = "yes",
  family = mixture(bernoulli, bernoulli),
  control = list(adapt_delta = 0.99)
)

m7 <- add_criterion(m7, criterion = "waic")



# Comparing models --------------------------------------------------------

model_weights(m0, m1, m2, m3, m4, m5, m6, weights = "waic") %>% round(5)

pp_check(m4, type = "bars_grouped", group = "id", nsamples = 200)





