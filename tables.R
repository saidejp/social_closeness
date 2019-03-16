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
             format = "latex",
             longtable = TRUE,
             #padding = 0, 
             digits = 2,
             col.names = c("Term", "Estimate", "SE", "L-95% CI", "U-95% CI"))



# Tidying mixture model output --------------------------------------------

load("mixture_promises.RData")

m_2_tidy <- ggmcmc::ggs(post_m_2) %>% 
  filter(Parameter %in% c("lambda1", "lambda2", "mu1", "mu2", "phi"))


m_2_tidy %>% 
  group_by(Parameter) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            l_95 = quantile(value, .025),
            u_95 = quantile(value, .975)) %>% 
  knitr::kable("markdown",
               padding = 0, 
               digits = 2,
               col.names = c("Parameter", "Mean", "SD", "L-95% CI", "U-95% CI")) 


# Tidying reaction time model output --------------------------------------

m_rt_tidy <- broom::tidy(m_rt) %>%  slice(1:4)

knitr::kable(m_rt_tidy,
             format = "markdown",
             digits = 2,
             longtble = TRUE,
             col.names = c("Term", "Estimate", "SE", "L-95% CI", "U-95% CI"))


