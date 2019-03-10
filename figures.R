
# Figures -----------------------------------------------------------------


# Packages ----------------------------------------------------------------


library(pacman)
p_load(magrittr, dplyr, forcats, tidyr, modelr, tidybayes,
       ggstance, ggridges, rstan, brms, tidyverse, bayesplot, rjags, 
       gridExtra, ggthemes)



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
         device = "jpeg",
         width = 10,
         height = 7,
         units = "in",
         file = paste0(names(g), ".jpeg"),
         plot = g)
  )


# Ploting mixtures --------------------------------------------------------

load("mixture_promise.RData")

ggsave("g16.jpeg",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = thetas,
       device = "jpeg",
       width = 10,
       height = 7,
       units = "in")


# Figure 1 ----------------------------------------------------------------

g1 <- g1 + labs(tag = "A", x = "") 
g4 <- g4 + labs(tag = "B")

g1_g4 <- grid.arrange(g1, g4, ncol = 2)

ggsave("fig1.jpeg",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = g1_g4,
       device = "jpeg",
       width = 10,
       height = 5,
       units = "in")



# Figure 3 ----------------------------------------------------------------

g5 <- g5 + labs(tag = "A", x = "Estimate", y = "") +
  theme(axis.text.y = element_text(size = 8))
          

g8 <- g8 + labs(tag = "B")

fig3 <- grid.arrange(g5, g8, ncol = 2)

ggsave("fig3.jpeg",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = fig3,
       device = "jpeg",
       width = 10,
       height = 5,
       units = "in")



# Figure 5 ----------------------------------------------------------------

bayesplot_theme_set(theme_bw(base_size = 15) +
                    theme(panel.grid.major = element_line(linetype = "blank"), 
                          panel.grid.minor = element_line(linetype = "blank")))

f_partner <- pp_check(m, 
                      type = "bars_grouped", 
                      nsamples = 50, 
                      group = "partner") +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("No", "Yes"),
                     name = "Pay back") 


f_promise <- pp_check(m, 
                      type = "bars_grouped", 
                      nsamples = 50, 
                      group = "promise") + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("No", "Yes"),
                     name = "Pay back")


ggsave("fig5.jpeg",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = f_partner,
       device = "jpeg",
       width = 10,
       height = 5,
       units = "in")


# Figure 6 ----------------------------------------------------------------

thetas <- thetas + labs(tag = "A")
g10 <- g10 + labs(tag = "B")

fig6 <- gridExtra::grid.arrange(thetas, g10, ncol = 2)


ggsave("fig6.jpeg",
       path = "~/Documents/R/github_Said/social_closeness/Manuscript/figures/",
       plot = fig6,
       device = "jpeg",
       width = 10,
       height = 5,
       units = "in")
