library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rstan)
library(rstanarm)
library(stringr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

theme_set(theme_light())

kplot <- function(x) {
  plot(x$pareto_k, pch = 3, col="blue", cex = 1.5, cex.lab=1.5, cex.axis = 1.3, xlab="Data point", ylab="Shape parameter k", ylim = c(min(x$pareto_k), max(x$pareto_k)+0.1))
  abline(a = 0, b=0, col = "grey", lty = 3)
  abline(a = 0.7, b=0, col = "red", lty = 4)
}

selected_vars <- read_csv("data/selected_vars.csv")

nytx_df <- selected_vars %>% 
  filter(ST %in% c(36, 48),
         ! Major_Category == "Interdisciplinary") %>% 
  mutate(state = if_else(ST == 36, "NY", "TX"),
         sex = if_else(SEX == 1, "male", "female"))

model_df1 <- nytx_df %>% 
  filter(Major_Category %in% c("Education", "Health", "Business")) %>% 
  filter(real_wage > 0) %>% 
  mutate(log_earnings = log(real_total_earn),
         log_wage = log(real_wage))

data_intervals_mean <- model_df1 %>% 
  filter(AGEP >= 20,
         AGEP <= 75) %>% 
  group_by(AGEP, sex, Major_Category, state) %>% 
  summarise(mean_wage = mean(real_wage, na.rm = T),
            sd_wage = sd(real_wage, na.rm = T),
            entries = n()) %>% 
  ungroup() %>% 
  mutate(sem = sd_wage / sqrt(entries),
         median = mean_wage * 1,
         q_upper = mean_wage + 2 * sd_wage,
         model = "data")

data_intervals_median <- model_df1 %>% 
  filter(AGEP >= 20,
         AGEP <= 75) %>% 
  group_by(AGEP, sex, Major_Category, state) %>% 
  summarise(median = median(real_wage, na.rm = T),
            q_upper = quantile(real_wage, 0.975, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(model = "data")

data_intervals_mean %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = mean_wage, colour = sex)) +
  geom_line(aes(y = mean_wage + 2 * sd_wage, colour = sex), linetype = "dashed") +
  ggtitle("Real data: wages (mean + 2 standard deviations)") +
  xlab("age") +
  ylab("wages in USD") +
  facet_grid(state ~ Major_Category)

cats_nytx <- model_df1 %>% 
  group_by(Major_Category, state, sex) %>% 
  summarise(entries = n()) %>% 
  spread(key = "sex", value = "entries")

lin_df <- model_df1 %>% 
  filter(AGEP >= 25,
         AGEP <= 50)

prototype_spline_df <- model_df1 %>%
  sample_n(size = 5000)

prototype_lin_df <- lin_df %>% 
  sample_n(size = 5000)

# prototype_lin_tx <- lin_df %>% # TX in those three categories has 52708 entries
#   filter(state == "TX") %>% 
#   sample_n(size = 3000) 
#   
# prototype_lin_ny <- lin_df %>% # NY in those three categories has 46058 entries
#   filter(state == "NY") %>% 
#   sample_n(size = 3000) 


post_sample_grid <- expand.grid(AGEP = 25:50, 
                                sex = c("male", "female"), 
                                Major_Category = c("Education", "Health", "Business"), 
                                state = c("NY", "TX"),
                                KEEP.OUT.ATTRS = F,
                                stringsAsFactors = F)

### Sexpipe2 ---------
log_sexpipe2 <- stan_glmer(log_wage ~ (AGEP | sex) + (1 | Major_Category) + (1 | state),
                           data = prototype_lin_df, family = gaussian())

log_post_sexpipe2 <- posterior_predict(log_sexpipe2, post_sample_grid)
exp_post_sexpipe2 <- exp(log_post_sexpipe2)

post_df <- bind_cols(post_sample_grid, as.data.frame(t(exp_post_sexpipe2)))

post_long <- post_df %>% 
  gather(key = "sample_no", value = "predicted_earning", V1:V4000)

post_gender <- post_long %>% 
  spread(key = "sex", value = "predicted_earning") %>% 
  mutate(ratio_mf = male / female,
         diff_mf = male - female)

ci_post_pred <- post_long %>% 
  group_by(AGEP, sex, Major_Category, state) %>% 
  summarise(q_lower = quantile(predicted_earning, 0.025, names = F),
            median = quantile(predicted_earning, 0.5, names = F),
            q_upper = quantile(predicted_earning, 0.975, names = F)) %>% 
  mutate(model = "linear")

ci_post_pred %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = median, colour = sex)) +
  geom_line(aes(y = q_lower, colour = sex), linetype = "dashed") +
  geom_line(aes(y = q_upper, colour = sex), linetype = "dashed") +
  ggtitle("Posterior predictive 95% intervals: yearly wages") +
  xlab("age") +
  ylab("wages in USD") +
  facet_grid(state ~ Major_Category)

ci_diff<- post_gender %>% 
  group_by(AGEP, Major_Category, state) %>% 
  summarise(q_lower = quantile(diff_mf, 0.025, names = F),
            median = quantile(diff_mf, 0.5, names = F),
            q_upper = quantile(diff_mf, 0.975, names = F))

ci_diff %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = median)) +
  geom_line(aes(y = q_lower), linetype = "dashed") +
  geom_line(aes(y = q_upper), linetype = "dashed") +
  ggtitle("Posterior predictive 95% intervals: difference in wages (male - female)") +
  xlab("age") +
  ylab("difference in wages [USD]") +
  facet_grid(state ~ Major_Category) 

ci_ratio <- post_gender %>% 
  group_by(AGEP, Major_Category, state) %>% 
  summarise(q_lower = quantile(ratio_mf, 0.025, names = F),
            median = quantile(ratio_mf, 0.5, names = F),
            q_upper = quantile(ratio_mf, 0.975, names = F))

ci_ratio %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = log2(median))) +
  geom_line(aes(y = log2(q_lower)), linetype = "dashed") +
  geom_line(aes(y = log2(q_upper)), linetype = "dashed") +
  ggtitle("Posterior predictive 95% intervals: log2 ratio of wages (male / female)") +
  xlab("age") +
  ylab("log2 ratio of wages [USD]") +
  facet_grid(state ~ Major_Category) 

# Leave one out cross-validation
link_log_sexpipe2 <- rstanarm::log_lik(log_sexpipe2)
loo_log_sexpipe2 <- rstanarm::loo(link_log_sexpipe2)
kplot(loo_log_sexpipe2)

plot(log_sexpipe2, plotfun = "trace", 
     # pars = c("(Intercept)",  "AGEP", "Major_CategoryEducation", "Major_CategoryHealth", "sigma"), 
     inc_warmup = FALSE)

### GAMM ---------
log_GAMM <- stan_gamm4(log_earnings ~ s(AGEP), 
                       random = ~(sex | Major_Category) + (sex | state), 
                       data = prototype_spline_df, family = gaussian(),
                       prior_intercept = NULL,
                       prior_smooth = NULL)

GAMM_sample_grid <- expand.grid(AGEP = 20:75, 
                                sex = c("male", "female"), 
                                Major_Category = c("Education", "Health", "Business"), 
                                state = c("NY", "TX"),
                                KEEP.OUT.ATTRS = F,
                                stringsAsFactors = F)

log_post_GAMM <- posterior_predict(log_GAMM, GAMM_sample_grid)
exp_post_GAMM <- exp(log_post_GAMM)

post_GAMM_df <- bind_cols(GAMM_sample_grid, as.data.frame(t(exp_post_GAMM)))

post_GAMM_long <- post_GAMM_df %>% 
  gather(key = "sample_no", value = "predicted_earning", V1:V4000)

post_GAMM_gender <- post_GAMM_long %>% 
  spread(key = "sex", value = "predicted_earning") %>% 
  mutate(ratio_mf = male / female)

ci_post_GAMM_pred <- post_GAMM_long %>% 
  group_by(AGEP, sex, Major_Category, state) %>% 
  summarise(q_lower = quantile(predicted_earning, 0.025, names = F),
            median = quantile(predicted_earning, 0.5, names = F),
            q_upper = quantile(predicted_earning, 0.975, names = F)) %>% 
  mutate(model = "splines")

ci_post_GAMM_pred %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = median, colour = sex)) +
  geom_line(aes(y = q_lower, colour = sex), linetype = "dashed") +
  geom_line(aes(y = q_upper, colour = sex), linetype = "dashed") +
  ggtitle("Posterior predictive 95% intervals: yearly earnings") +
  xlab("age") +
  ylab("total yearly earnings in USD") +
  facet_grid(state ~ Major_Category)

ci_GAMM_ratio <- post_GAMM_gender %>% 
  group_by(AGEP, Major_Category, state) %>% 
  summarise(q_lower = quantile(ratio_mf, 0.025, names = F),
            median = quantile(ratio_mf, 0.5, names = F),
            q_upper = quantile(ratio_mf, 0.975, names = F))

ci_GAMM_ratio %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = log2(median))) +
  geom_line(aes(y = log2(q_lower)), linetype = "dashed") +
  geom_line(aes(y = log2(q_upper)), linetype = "dashed") +
  ggtitle("Posterior predictive 95% intervals: log2 ratio in real earnings (male / female)") +
  xlab("age") +
  ylab("log2 ratio in total yearly earnings [USD]") +
  facet_grid(state ~ Major_Category) 

# Leave one out cross-validation
link_log_GAMM <- rstanarm::log_lik(log_GAMM)
loo_log_GAMM <- rstanarm::loo(link_log_GAMM)
kplot(loo_log_GAMM)

plot(log_GAMM, plotfun = "trace", 
     # pars = c("(Intercept)",  "AGEP", "Major_CategoryEducation", "Major_CategoryHealth", "sigma"), 
     inc_warmup = FALSE)

bind_rows(ci_post_GAMM_pred, ci_post_pred) %>% 
  mutate(plot_cat = paste0(str_sub(sex, 1, 1), " (", model, ")")) %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = median, colour = plot_cat)) +
  geom_line(aes(y = q_lower, colour = plot_cat), linetype = "dashed") +
  geom_line(aes(y = q_upper, colour = plot_cat), linetype = "dashed") + 
  scale_colour_manual(values = c("#e31a1c", "#fd8d3c", "#08306b", "#2171b5"),
                      name = "group") +
  ggtitle("Posterior predictive 95% intervals: yearly earnings") +
  xlab("age") +
  ylab("total yearly earnings in USD") +
  facet_grid(state ~ Major_Category)

bind_rows(ci_post_pred, data_intervals_median) %>% 
  mutate(plot_cat = paste0(str_sub(sex, 1, 1), " (", model, ")")) %>% 
  ggplot(., aes(x = AGEP)) +
  geom_line(aes(y = median, colour = plot_cat)) +
  geom_line(aes(y = q_upper, colour = plot_cat), linetype = "dashed") + 
  scale_colour_manual(values = c("#e31a1c", "#fd8d3c", "#08306b", "#2171b5"),
                      name = "group") +
  ggtitle("Posterior predictive 95% intervals: yearly wages") +
  xlab("age") +
  ylab("wages in USD") +
  facet_grid(state ~ Major_Category)

### Sexpipe3 ---------
log_sexpipe3 <- stan_glmer(log_earnings ~ (AGEP | sex) + (1 | Major_Category) + (1 | state),
                           data = prototype_lin_df, family = gaussian())


# agg_nytx_df <- nytx_df %>% 
#   group_by(Major_Category, state, sex, AGEP) %>% 
#   summarise(mean_total_earning = mean(real_total_earn, na.rm = T),
#             mean_wage = mean(real_wage, na.rm = T),
#             sd_total_earning = sd(real_total_earn, na.rm = T),
#             entries = n()) %>% 
#   ungroup() %>% 
#   mutate(sem = sd_total_earning / sqrt(entries)) %>% 
#   filter(AGEP >= 25,
#          AGEP <= 70)
