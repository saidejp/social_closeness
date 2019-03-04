
# Promise and Social Closeness --------------------------------------------

#+ setup, message = FALSE, warning = FALSE, error = FALSE

# Packages ----------------------------------------------------------------

library(pacman)
p_load(magrittr, dplyr, forcats, tidyr, modelr, tidybayes,
       ggstance, ggridges, rstan, brms, tidyverse, bayesplot)


# Loading data ------------------------------------------------------------

load("social_closeness.RData")


# Structure data ----------------------------------------------------------
str(data)


# Visualization -----------------------------------------------------------


# Theme -------------------------------------------------------------------

my_theme <- theme_bw(base_size = 15) + 
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank")) 



# How much do participants pay back? --------------------------------------

(g1 <- data %>% 
  filter(!is.na(decision)) %>% 
  ggplot(aes(x = decision)) +
  geom_bar(fill = "#053778") +
  labs(x = "Decision",
       y = "Count") +
  my_theme)


# Pay back to each partner ------------------------------------------------

(g2 <- data %>% 
  filter(!is.na(decision)) %>% 
  ggplot(aes(x = partner, fill = decision)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#A4CAE3", "#053778")) +
  labs(x = "",
       y = "Percent",
       fill = "") +
   my_theme + 
   theme(legend.position = "bottom")) 

# By promise condition ----------------------------------------------------

(g3 <- data %>% 
  filter(!is.na(decision)) %>% 
  ggplot(aes(x = partner, fill = decision)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#A4CAE3", "#053778")) +
  facet_grid(~promise) +
  labs(x = "",
       y = "Percent",
       fill = "") +
  my_theme +
  theme(legend.position = "bottom"))


# Frecuencies of promises -------------------------------------------------

(g4 <- data %>% 
  filter(!is.na(prom_keys),
         prom_keys != "No response") %>% 
  ggplot(aes(x = prom_keys)) +
  geom_bar(fill = "#053778") +
  labs(x = "",
       y = "Count") +
  my_theme)
  

# Modeling ----------------------------------------------------------------

cases <- data %>% 
  filter(dec_partner == "Trust") %>% 
  select(id, partner, promise, response)


# Priors ------------------------------------------------------------------

prior <- c(set_prior("normal(0, 5)", class = "b"), 
           set_prior("cauchy(0, 2)", class = "sd"),
           set_prior("lkj(2)", class = "cor"))


# Fitting the model -------------------------------------------------------

(m <- brm(response ~ promise + partner + (promise + partner | id), 
         data = cases,
         family = bernoulli("logit"), 
         prior = prior,
         sample_prior = "yes",
         chains = 4, 
         control = list(adapt_delta = 0.95)))


# Hypothesis testing ------------------------------------------------------

hypothesis(m, "promisePromise > 0", class = "b")
hypothesis(m, "partnerFriend - partnerConfederate > 0", class = "b") 


# Posterior Samples -------------------------------------------------------

posteriors <- posterior_samples(m)


# Beta posteriors ---------------------------------------------------------

b_posteriors <- select(posteriors, 1:4)
names(b_posteriors) <- c("Intercept", "Promise", "Confederate", "Friend")

(g5 <- bayesplot::mcmc_areas(
  b_posteriors,
  point_est = "mean") +
  geom_vline(xintercept = 0, 
             col = "black", 
             lty = 2, 
             alpha = 0.2) +
  my_theme)


# Posterior predictive for id ---------------------------------------------
pp_check(m, type = "bars_grouped", nsamples = 100, group = "id")


# Posterior predictive for partner ----------------------------------------
pp_check(m, type = "bars_grouped", nsamples = 50, group = "partner")


# Posterior predictive for promise ----------------------------------------
pp_check(m, type = "bars_grouped", nsamples = 50, group = "promise")



# Lineal Predictor --------------------------------------------------------
 

# Coeficients -------------------------------------------------------------

Intercept <- mean(posteriors$b_Intercept)
promisePromise  <- mean(posteriors$b_promisePromise)
partnerConfederate <- mean(posteriors$b_partnerConfederate)
partnerFriend <-  mean(posteriors$b_partnerFriend)



# Indicator variables -----------------------------------------------------

cases_1 <- cases %>% 
  mutate(promise_1 = if_else(promise == "Promise", 1, 0),
         confederate = if_else(partner == "Confederate", 1, 0),
         friend = if_else(partner == "Friend", 1, 0))


# Combining predictors in one  --------------------------------------------

cases_1 <- cases_1 %>% 
  mutate(
    sum_pred = Intercept + promisePromise * promise_1 +
      partnerConfederate * confederate + partnerFriend * friend,
    lin_pred = 1 / (1 + exp( -sum_pred )) # Probabilities from 0 to 1
    )



# Population effects in all subjects --------------------------------------

(g6 <- ggplot(cases_1, aes(x = sum_pred, y = lin_pred)) +
   stat_function(fun = function(x) { 1 / ( 1 + exp(-x) ) },
                 col = "#053778",
                 size = .9) +
   geom_jitter(aes(y = response), 
               width = 1, 
               height = .09,
               alpha = 0.5,
               col = "#053778") +
   labs(x = "Linear predictor",
        y = "Pr (Pay back)") +
   #facet_wrap(~ id) +
   my_theme)


# Population effects in each subject --------------------------------------


(g7 <- ggplot(cases_1, aes(x = sum_pred, y = lin_pred)) +
  stat_function(fun = function(x) { 1 / ( 1 + exp(-x) ) },
                col = "#053778",
                size = .9) +
  geom_jitter(aes(y = response), 
              width = 1, 
              height = .09,
              alpha = 0.5,
              col = "#053778") +
  labs(x = "Linear predictor",
       y = "Pr(Pay back)") +
  facet_wrap(~ id) +
  my_theme)



# Population effects with uncertainty -------------------------------------

b_posteriors_100 <- b_posteriors %>% 
  sample_n(100) %>% 
  mutate(id = 1:100) %>% 
  select(id, everything())


(g8 <- pmap(b_posteriors_100, 
     function(id, Intercept, Promise, Confederate, Friend) {
       stat_function(data = . %>% mutate(id = id),
                     aes(group = id),
                     fun = function(x) { 1 / (1 + exp( -(Intercept +
                                                          Promise * x +
                                                          Confederate * x +
                                                          Friend * x) ))},
                     alpha = 0.09, 
                     col = "#A4CAE3", 
                     lty = 1) }) %>% 
  reduce(.init = ggplot(cases_1, aes(x = lin_pred, y = response)), `+`) +
  stat_function(fun = function(x) { 1 / (1 + exp(-( Intercept +
                                                      promisePromise * x +
                                                      partnerConfederate * x +
                                                      partnerFriend * x ))) }, 
                size = 1.1, 
                col = "#053778") +
  xlim(-0.5, 1) +
  labs(x = "Linear predictor",
       y = "Pr(pay back)") +
  my_theme)


# Effects by subjects -----------------------------------------------------

set.seed(12)

# Partial pooling
random_effects <- m %>% 
  spread_draws(r_id[subject, term]) %>% 
  spread(term, r_id) %>% 
  sample_n(50) %>%  # Using 50 posterior samples
  mutate(id = 1:n()) %>% 
  select(-c(1:3)) %>% 
  select(id, everything()) %>% 
  ungroup()


# Adjusting population parameters by subject ------------------------------

random_effects_2 <- random_effects %>% 
  transmute(id = id,
            subject = factor(subject),
            Intercept = .$Intercept + mean(b_posteriors$Intercept),
            Promise = .$promisePromise + mean(b_posteriors$Promise),
            Confederate = .$partnerConfederate + mean(b_posteriors$Confederate),
            Friend = .$partnerFriend + mean(b_posteriors$Friend))

(g9 <- pmap(random_effects_2, .f = 
       function(id, subject, Intercept, Promise, Confederate, Friend) {
         stat_function(data = . %>% mutate(id = id, subject = subject),
                       aes(group = id),
                       fun = function(x) { 1 / (1 + exp( -(Intercept +
                                                            Promise * x +
                                                            Confederate * x +
                                                            Friend * x) ))},
                       alpha = 0.015, 
                       col = "#053778") 
       }) %>% 
  reduce(.init = ggplot(cases_1, aes(x = lin_pred, y = response)) + facet_wrap(~subject), `+`) +
  xlim(-0.5, 1) +
  guides(col = FALSE) +
  labs(x = "Linear Predictor",
       y = "Pr(pay back)") +
  my_theme)


# Two cases from two latent thetas ----------------------------------------
# Participant 17 and 5

(g10 <- cases %>% 
  filter(id %in% c(5, 17)) %>% 
  group_by(id) %>% 
  mutate(trial = 1:18,
         decision = as.factor(
           if_else(response == 1, 
                   "Pay back", 
                   "Not to pay back") )) %>%
  ungroup() %>% 
  ggplot(aes(x = trial, y = response)) +
  geom_line(aes(group = 1, col = decision)) +
  geom_point(aes(col = decision), size = 2.5) +
  facet_wrap(~id) +
  scale_x_continuous(breaks = c(1:18)) +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = "Trials",
       y = "Response",
       col = "") +
  scale_color_manual(values = c("#053778", "#A4CAE3")) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 7))) 


# Strategies from two groups ----------------------------------------------

dishonest <- c(1, 2, 4, 5, 8, 10, 12, 13, 15, 19)
honest <- c(3, 6, 7, 9, 11, 14, 16, 17, 18, 20)

(g11 <- cases %>% 
  group_by(id) %>% 
  mutate(trial = 1:18,
         group = if_else(id %in% honest, "Honest", "Dishonest")) %>% 
  ungroup() %>% 
  ggplot(aes(x = trial, y = response)) +
  geom_line(aes(group = 1, col = group)) +
  geom_point(aes(col = group), size = 2.5) +
  facet_wrap(~id) +
  scale_x_continuous(breaks = c(1:18)) +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = "Trials",
       y = "Response",
       col = "") +
  scale_color_manual(values = c("#053778", "#A4CAE3")) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 5))) 


# Counts by group ---------------------------------------------------------

(g12 <- cases %>% 
  group_by(id) %>% 
  mutate(trial = 1:18,
         group = if_else(id %in% honest, "Honest", "Dishonest"),
         response = factor(response, 
                           levels = c(0, 1), 
                           labels = c("Not to pay back", "Pay back"))) %>% 
  ungroup() %>% 
  filter(!is.na(response)) %>% 
  ggplot(aes(x = response, fill = group)) +
  geom_bar() +
  scale_fill_manual(values = c("#053778", "#A4CAE3")) +
  labs(x = "",
       y = "Count") +
  guides(fill = FALSE) +
  facet_wrap(~ group) +
  my_theme)


# Counts by group and promise ---------------------------------------------

(g12_b <- cases %>% 
  group_by(id) %>% 
  mutate(trial = 1:18,
         group = if_else(id %in% honest, "Honest", "Dishonest"),
         response = factor(response, 
                           levels = c(0, 1), 
                           labels = c("Not to pay back", "Pay back"))) %>% 
  ungroup() %>% 
  filter(!is.na(response)) %>% 
  ggplot(aes(x = response, fill = group)) +
  geom_bar() +
  scale_fill_manual(values = c("#053778", "#A4CAE3")) +
  labs(x = "",
       y = "Count") +
  guides(fill = FALSE) +
  facet_grid(group ~ promise) +
  my_theme +
  theme(axis.text.x = element_text(size = 10)))


# Reaction times between groups -------------------------------------------

data <- data %>% 
  mutate(group = if_else(id %in% dishonest, "Dishonest", "Honest")) %>% 
  select(id, group, everything())


(g13 <- data %>% 
  filter(dec_partner == "Trust") %>% 
  ggplot(aes(x = group, y = decision_rt, fill = group, col = group)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(alpha = 0.8,
             width = 0.05,
             show.legend = F) +
  scale_fill_manual(values = c("#053778", "#A4CAE3")) +
  scale_color_manual(values = c("#053778", "#A4CAE3")) +
  scale_y_continuous(limits = c(0, 10)) +
  coord_flip() +
  labs(x = "", 
       y = "Reaction time (s)", 
       fill = "",
       group = "") +
  guides(col = FALSE,
         fill = FALSE) +
  my_theme) 

  
# Reaction times by partner -----------------------------------------------

(g14 <- data %>% 
    filter(dec_partner == "Trust") %>% 
    ggplot(aes(x = group, y = decision_rt, fill = group, col = group)) +
    geom_violin(alpha = 0.5) +
    geom_jitter(alpha = 0.8,
                width = 0.05,
                show.legend = F) +
    scale_fill_manual(values = c("#053778", "#A4CAE3")) +
    scale_color_manual(values = c("#053778", "#A4CAE3")) +
    scale_y_continuous(limits = c(0, 10)) +
    facet_wrap(~partner) +
    labs(x = "", 
         y = "Reaction time (s)", 
         fill = "",
         group = "") +
    guides(col = FALSE,
           fill = FALSE) +
    my_theme) 


# Comparison of reaction times --------------------------------------------


# Data for model m_rt -----------------------------------------------------

cases_2 <- data %>% 
  filter(dec_partner == "Trust") %>% 
  select(id, group, decision_rt)


# Prior m_rt --------------------------------------------------------------

prior_rt <- set_prior("normal(0, 5)", class = "b")


# Fitting m_rt ------------------------------------------------------------

(m_rt <- brm(decision_rt ~ -1 + group + (1 | id),
            data = cases_2,
            prior = prior_rt,
            sample_prior = "yes"))


# Hypothesis testing: mu_d = mu_h -----------------------------------------

hypothesis(m_rt, "groupDishonest - groupHonest = 0") 
hypothesis(m_rt, "groupDishonest - groupHonest < 0") 


# Point estimates by groups -----------------------------------------------

marginal_effects(m_rt)


# Posterior predictive m_2 ------------------------------------------------

pp_check(m_rt, nsamples = 500, group = "group", type = "stat_grouped")


# Mu’s data frame ---------------------------------------------------------

mu_posteriors <- posterior_samples(m_rt) %>% 
  select(mu_dishonest = b_groupDishonest, 
         mu_honest = b_groupHonest) %>% 
  mutate(mu_diff = mu_dishonest - mu_honest) %>% 
  as_tibble()

(g15 <- ggplot(mu_posteriors, aes(x = mu_diff)) +
  geom_histogram(fill = "#A4CAE3",
                 col = "#A4CAE3",
                 alpha = 0.4) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = "#787878",
             size = .6) +
  stat_pointintervalh(aes(y = 0),
                      .width = .95,
                      size = 10,
                      col = "#053778") +
  labs(x = expression(mu[Dishonest] - mu[Honest]),
       y = "Count") +
  my_theme)




