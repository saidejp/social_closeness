# Varying effects by promises ----


# Data --------------------------------------------------------------------

load("~/Documents/R/github_Said/website/said/public/candidatura/candidatura.RData")


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, brms, tidybayes)


# Pay by promise level and partner ----------------------------------------

(g14 <- d_ord %>% 
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
  theme(legend.position = "top", 
        axis.text.x = element_text(size = 10)))


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



(g13b <- ggplot(d_g13, aes(x = part_ord, y = rate, col = case))  +
  geom_hline(yintercept = 0.5, lty = 2, col = "gray") +
  geom_pointrange(aes(ymax = rate + pred_sd, ymin = rate - pred_sd),
                  position = position_dodge(width = .5)) +
  facet_wrap(~prom_ord) +
  scale_color_manual(values = c("#053778", "#A4CAE3")) +
  labs(x = NULL,
       y = "Rate",
       col = NULL) +
  my_theme +
  theme(legend.position = c(0.83, 0.25), 
        axis.text.x = element_text(size = 10)))



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

data4 <- data3 %>% 
  mutate(group = ifelse(id %in% high, "High", "Low") %>% 
           factor(levels = c("High", "Low"))) %>% 
  mutate(prom_ord = factor(prom_ord, ordered = T))

data4 %>% 
  group_by(group) %>% 
  summarise(mean = mean(response),
            sd = sd(response),
            sum = sum(response),
            n = n())

prop.test(c(286, 240), n = c(342, 414))


data4 %>% 
  count(group, prom_ord)

# Qué tan deshonestos podríamos considerarlos si su compromiso es significativamente menor?
fit3 <- brm(prom_ord ~ group + (1 | id + trial), 
            data = data4,
            family = cumulative("probit"),
            sample_prior = "yes",
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.99, 
                           max_treedepth = 15))

marginal_effects(fit3, categorical = T)

hypothesis(fit3, "groupLow < 0")

diff_data <- posterior_samples(fit3) %>% 
  select(low = b_groupLow)

ggplot(diff_data, aes(x = low)) +
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
  labs(x = "Low",
       y = "Count") +
  my_theme



data5 <- data4 %>% 
  filter(prom_ord != "No response") %>% 
  mutate(prom_ord = fct_drop(prom_ord),
         group = factor(group, levels = c("Low", "High")))
  
fit4 <- brm(prom_ord ~ group + (1 | id + trial), 
            data = data5,
            family = cumulative("probit"),
            sample_prior = "yes",
            chains = 4,
            cores = 4,
            control = list(adapt_delta = 0.99, 
                           max_treedepth = 15))

plt <-  plot(marginal_effects(fit4, categorical = T))


plt$`group:cats__` +
  labs(x = NULL,
       col = "Promise",
       fill = "Promise") +
  scale_fill_manual(values = RColorBrewer::brewer.pal(9, "Blues")[c(3,5,7,9)], 
                    aesthetics = "col") +
  my_theme 


data %>% 
  mutate(ahead = lead(decision)) %>% 
  select(partner, dec_partner, decision, ahead) %>% 
  count(partner, dec_partner, ahead)


d_ord %>% 
  mutate(ahead = lead(prom_ord, n = 2L)) %>% 
  filter(ahead != "No response") %>% 
  select(dec_partner, ahead) %>% 
  count(dec_partner, ahead)
