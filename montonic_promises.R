
# Monotonic effect of promises and partners -------------------------------

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



# Labeling ordinal predictors ---------------------------------------------

part_levels <- c("Computer", "Confederate", "Friend")
prom_levels <- c("No response", "Never", "Sometimes", "Mostly", "Always")

d_ord <- d_ord %>% 
  mutate(
    part_ord = factor(part_ord, 
                      levels = c(0, 1, 2),
                      labels = part_levels,
                      ordered = T),
    prom_ord = factor(prom_ord,
                      levels = c(0, 1, 2, 3, 4),
                      labels = prom_levels,
                      ordered = T)
    )
  

# Descriptives ------------------------------------------------------------

d_ord %>% 
  count(part_ord, prom_ord, not_pay) %>% 
  filter(!is.na(not_pay), 
         prom_ord != "No response") %>% 
  mutate(not_pay = factor(not_pay, 
                          levels = c(1, 0),
                          labels = c("Not to pay back", "Pay back"))) %>% 
  ggplot(aes(x = prom_ord, y = n, fill = not_pay)) +
  geom_col(position = "dodge") +
  labs(x = "Promise",
       y = "Count",
       fill = "") +
  scale_fill_manual(values = c("#053778", "#A4CAE3")) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"))


# Model -------------------------------------------------------------------

prior <- prior(normal(0, 5), class = "b") +
  prior(normal(0, 5), class = "b", coef = "mopart_ord") +
  prior(normal(0, 5), class = "b", coef = "moprom_ord") +
  prior(normal(0, 5), class = "b", coef = "moprom_ord:mopart_ord") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "mopart_ord1") +
  prior(dirichlet(1, 1, 1, 1), class = "simo", coef = "moprom_ord:mopart_ord1") +
  prior(dirichlet(1, 1), class = "simo", coef = "moprom_ord:mopart_ord2")

m <- brm(
  not_pay ~ mo(prom_ord) * mo(part_ord) + (mo(prom_ord) *  mo(part_ord) | id),
  data = d_ord,
  prior = prior,
  sample_prior = "yes",
  family = bernoulli("logit"),
  control = list(adapt_delta = 0.95)
)

# Marginal effects prom * partner -----------------------------------------

p <- plot(marginal_effects(m)) 

p$`prom_ord:part_ord` +
  labs(x = "",
       y = "Pr(Not to pay back)",
       fill = "",
       col = "") +
  scale_fill_manual(values = c("#A4CAE3", "#4F94CD", "#053778")) +
  scale_color_manual(values = c("#A4CAE3", "#4F94CD", "#053778")) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"),
        legend.position = "bottom")



