
# Figures -----------------------------------------------------------------


# Packages ----------------------------------------------------------------


library(pacman)
p_load(magrittr, dplyr, forcats, tidyr, modelr, tidybayes,
       ggstance, ggridges, rstan, brms, tidyverse, bayesplot, rjags)



# Data --------------------------------------------------------------------

load("social_closeness.RData")


# Figure 1 ----------------------------------------------------------------
ggsave("f1.tiff",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = g1,
       device = "tiff",
       width = 9,
       height = 7,
       units = "in")


# Saving multiple plots into separate files -------------------------------

g <- mget(ls(pattern = "g")) 

invisible(
  mapply(ggsave, 
         path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
         device = "tiff",
         width = 9.5,
         height = 7,
         units = "in",
         file = paste0(names(g), ".tiff"),
         plot = g)
  )


# Ploting mixtures --------------------------------------------------------

load("mixture_promise.RData")

ggsave("g16.tiff",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = thetas,
       device = "tiff",
       width = 9.5,
       height = 7,
       units = "in")


