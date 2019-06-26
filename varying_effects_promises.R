# Varying effects by promises ----


# Data --------------------------------------------------------------------

load("~/Documents/R/github_Said/website/said/public/candidatura/candidatura.RData")


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, brms)


# Pay by promise level and partner ----------------------------------------

d_ord %>% 
  mutate(pay_back = ifelse(response == 1, "Pay back", "Not to pay back")) %>% 
  filter(dec_partner == "Trust") %>% 
  ggplot(aes(x = prom_ord, fill = pay_back)) +
  geom_bar(position = "fill") + 
  labs(fill = NULL,
       y = "Percent",
       x = NULL) +
  facet_wrap(~part_ord) +
  scale_fill_manual(values = c("#A4CAE3", "#053778")) +
  my_theme +
  coord_flip() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 10)) 


# Model: varying effects by promise level ---------------------------------

data3 <- d_ord %>% 
  filter(dec_partner == "Trust") %>% 
  mutate(prom_ord = factor(prom_ord, ordered = F),
         part_ord = factor(part_ord, ordered = F),
         response = as.integer(response))

prior2 <- c(set_prior("normal(0, 2)", class = "b"), 
           set_prior("cauchy(0, 2)", class = "sd"),
           set_prior("lkj(2)", class = "cor"))

fit <- brm(response ~ part_ord + (part_ord | prom_ord),
           data = data3,
           family = bernoulli("logit"),
           prior = prior2,
           sample_prior = "yes",
           chains = 4,
           cores = 4,
           control = list(adapt_delta = 0.99))


# Hypothesis --------------------------------------------------------------

hypothesis(fit, "part_ordFriend - part_ordConfederate > 0")
hypothesis(fit, "part_ordConfederate > 0")



# Posterior Predictive ----------------------------------------------------

pp_check(fit, type = "bars_grouped", group = "prom_ord", nsamples = 30)


pp_2 <- posterior_predict(fit, nsamples = 100) %>% 
  as_tibble()

pp_tidy_2 <- pp_2 %>% 
  gather(key = "row_cases",value = "prediction") %>% 
  mutate(row = gl(ncol(pp_2), k = 100) %>% as.numeric()) %>% 
  select(row, -row_cases, prediction)

pp_cases_2 <- data3 %>% 
  mutate(row = 1:nrow(data3)) %>% 
  select(row, everything()) %>% 
  right_join(pp_tidy_2, by = "row")


(d_g13 <- pp_cases_2 %>% 
  group_by(part_ord, prom_ord) %>% 
  summarise(Predicted = mean(prediction),
            Response = mean(response),
            pred_sd = sd(prediction)) %>% 
  gather(key = "case", value = "rate", -part_ord, -prom_ord, -pred_sd) %>% 
  ungroup() %>% 
  mutate(pred_sd = ifelse(case == "Response", 0, pred_sd)))



# Varying effects by promise level ----------------------------------------

(g13 <- ggplot(d_g13, aes(x = prom_ord, y = rate, col = case))  +
  geom_hline(yintercept = 0.5, lty = 2, col = "gray") +
  geom_pointrange(aes(ymax = rate + pred_sd, ymin = rate - pred_sd),
                  position = position_dodge(width = .5)) +
  facet_wrap(~part_ord) +
  scale_color_manual(values = c("#053778", "#A4CAE3")) +
  labs(x = NULL,
       y = "Rate",
       col = NULL) +
  my_theme +
  coord_flip() +
  theme(legend.position = "top", 
        axis.text.x = element_text(size = 10)))


# Clustering --------------------------------------------------------------

perc <- cases %>% 
  group_by(id) %>% 
  summarise(perc = mean(response)) 

x <- cases %>% 
  group_by(id) %>% 
  summarise(perc = mean(response)) %>% 
  pull(perc)

id <- cases %>% 
  group_by(id) %>% 
  summarise(perc = mean(response)) %>% 
  pull(id)

clust <- hclust(dist(x), method = "ward.D", members = id)
plot(clust)



high <- c(32, 15, 6, 13, 38, 36, 20, 19, 7, 14, 23, 28, 37, 42, 31, 3, 9, 10, 40)
low <- c(33, 16, 29, 12, 25, 30, 39, 27, 24, 22, 17, 11, 2, 5, 35, 26, 21, 1, 8, 
         41, 34, 4, 18)

data3 <- data3 %>% 
  mutate(group = ifelse(id %in% high, "Unselfish", "Selfish") %>% 
           factor(levels = c("Unselfish", "Selfish")))

data3 %>% 
  group_by(group) %>% 
  summarise(mean = mean(response),
            sd = sd(response),
            sum = sum(response),
            n = n())

prop.test(c(286, 240), n = c(342, 414))




# Analyzing broken promises -----------------------------------------------

broken <- d_ord %>% 
  filter(prom_ord == "Always", dec_partner == "Trust") %>% 
  mutate(broken = if_else(response == 1, 0, 1) %>% 
           as.integer(),
         part_ord = factor(part_ord, ordered = F))

  
broken %>% 
  group_by(part_ord) %>% 
  summarise(mean = mean(response),
            sd = sd(response),
            sum = sum(response),
            n = n(),
            broken = n - sum)


# Modeling Broken Promises ------------------------------------------------

fit2 <- brm(broken ~ -1 + part_ord + (-1 + part_ord | id),
            data = broken,
            family = bernoulli("logit"),
            prior = prior2,
            sample_prior = "yes",
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.95))



# BF for the effect of social closeness -----------------------------------

hypothesis(fit2, "part_ordComputer - ((part_ordConfederate + part_ordFriend) / 2) > 0")
hypothesis(fit2, "part_ordConfederate - part_ordFriend > 0")



# Estimates in probability scale ------------------------------------------

samples_fit2 <- posterior_samples(fit2)[, 1:3] %>% 
  mutate_all(inv_logit_scaled) 

names(samples_fit2) <- c("Computer", "Confederate", "Friend")


bayesplot::mcmc_intervals(
  samples_fit2,
  prob_outer = 0.97
  ) +
  labs(x = "Probability of a Broken Promise") +
  geom_vline(xintercept = 0, lty = 2, col = "gray") +
  my_theme


