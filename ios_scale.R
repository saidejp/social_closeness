
# IOS scale ---------------------------------------------------------------

pacman::p_load(brms, tidyverse, readxl)


# Data --------------------------------------------------------------------

load("~/Google Drive/PromesaConducta_8to2/data/promesas_analisis/Analisis-Comportamiento/promesas_analisis/ios_modeling.RData")


# Modeling ----------------------------------------------------------------

fit <- brm(
  response ~ partner + participant + (partner | id),
  data = data,
  family = cumulative("logit"),
  prior = set_prior("normal(0, 1)"),
  sample_prior = "yes",
  cores = 4,
  chains = 4,
  control = list(adapt_delta = 0.9)
)


# Checking ----------------------------------------------------------------

marginal_effects(fit, effects =  "participant", categorical = T)
hypothesis(fit, "participantfriend = 0")
