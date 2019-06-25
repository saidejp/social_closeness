# Varying effects by promises ----


# Data --------------------------------------------------------------------

load("~/Documents/R/github_Said/website/said/public/candidatura/candidatura.RData")


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, brms)


# Pay by promise level and partner ----------------------------------------

d_ord %>% 
  mutate(pay_back = ifelse(response == 1, "Pay back", "Not to pay back")) %>% 
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

  

