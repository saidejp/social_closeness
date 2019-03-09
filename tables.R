# Tables
library(pacman)
p_load(magrittr, dplyr, forcats, tidyr, modelr, tidybayes,
       ggstance, ggridges, rstan, brms, tidyverse, bayesplot)


# Loading data ------------------------------------------------------------

load("social_closeness.RData")


# Tidying model 1 output --------------------------------------------------
m_tidy <- broom::tidy(m) %>% 
  slice(1:4)

knitr::kable(m_tidy, 
             format = "markdown", 
             padding = 0, 
             digits = 2,
             col.names = c("Term", "Estimate", "SE", "L-95% CI", "U-95% CI"))

