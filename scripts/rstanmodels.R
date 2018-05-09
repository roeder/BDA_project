library(readr)
library(rstanarm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rstan)
setwd("~/Desktop/BSA_exam")


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Functions

kplot <- function(x) {
  plot(x$pareto_k, pch = 3, col="blue", cex = 1.5, cex.lab=1.5, cex.axis = 1.3, xlab="Data point", ylab="Shape parameter k", ylim = c(min(x$pareto_k), max(x$pareto_k)+0.1))
  abline(a = 0, b=0, col = "grey", lty = 3)
  abline(a = 0.7, b=0, col = "red", lty = 4)
}


plot_facets <- function(my_data) {
  ggplot(my_data, aes(x = AGEP)) +
    geom_ribbon(data = filter(my_data, SEX == 1),
                aes(ymin = mean_total_earning - 1.96 * sem,
                    ymax = mean_total_earning + 1.96 * sem),
                alpha = 0.2) +
    geom_ribbon(data = filter(my_data, SEX == 2),
                aes(ymin = mean_total_earning - 1.96 * sem,
                    ymax = mean_total_earning + 1.96 * sem),
                alpha = 0.2) + 
    geom_line(aes(y = mean_total_earning, colour = factor(SEX))) + 
    scale_colour_discrete(name = "sex", labels = c("male", "female")) +
    facet_wrap(~ Major_Category) +
    theme(legend.position = c(.95, 0), legend.justification = c(1, 0))
}




# Load and filter data


# ??? Should we keep negative total earn

selected_vars <- read_csv("selected_vars.csv")

norm_dist_df <- selected_vars %>% 
  group_by(Major_Category, AGEP, SEX, ST) %>% 
  summarise(mean_total_earning = mean(real_total_earn, na.rm = T),
            mean_wage = mean(real_wage, na.rm = T),
            sd_total_earning = sd(real_total_earn, na.rm = T),
            entries = n()) %>% 
  ungroup() %>% 
  mutate(sem = sd_total_earning / sqrt(entries)) %>% 
  filter(! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 70)


NYTX <- which(norm_dist_df$ST %in% c(36, 48))

norm_dist_df <- norm_dist_df[NYTX, ]


# Subset for testing
EHB_subset <- which(norm_dist_df$Major_Category %in% c("Education", "Health", "Business"))
EHB_subset <- as.data.frame(norm_dist_df[EHB_subset,])
EHB_subset$SEX <- as.factor(EHB_subset$SEX) 


### ----------------------------------------------------------------------------------------- ###


# Generalized additive mixed model, we model mte as a function of age (which has been smoothend with polynomial splines) 
# and sex and we add a hieracical term (random effect term) which is the Major Category.
plot_facets(EHB_subset)

# my_GAMM <- stan_gamm4(mean_total_earning ~ SEX + s(AGEP), random = ~(1|Major_Category), data = EHB_subset, family = gaussian(), adapt_delta = 0.99)
my_GAMM <- stan_gamm4(mean_total_earning ~ s(AGEP), random = ~(SEX|Major_Category), data = EHB_subset, family = gaussian(), adapt_delta = 0.99)

# Trace plot
plot(my_GAMM, plotfun = "trace", pars = c("(Intercept)", "smooth_sd[s(AGEP)1]", "smooth_sd[s(AGEP)2]"), inc_warmup = FALSE)

# Model fit plot
plot_nonlinear(my_GAMM)

# Leave one out cross-validation
linkGAMM <- rstanarm::log_lik(my_GAMM)
loo_GAMM <- rstanarm::loo(linkGAMM)
kplot(loo_GAMM)


### ----------------------------------------------------------------------------------------- ###

# Generalized linear model with ages only in the interval which is linear with hierarchical link.
EHB_subset_filt <- EHB_subset[EHB_subset$AGEP <= 45, ]
plot_facets(EHB_subset_filt)


### ----------------------------------------------------------------------------------------- ###


#my_GLM_H <- stan_glmer(mean_total_earning ~ SEX + AGEP + (1|Major_Category), data = EHB_subset_filt, family = gaussian(), adapt_delta = 0.99)
my_GLM_H <- stan_glmer(mean_total_earning ~ AGEP + (SEX|Major_Category), data = EHB_subset_filt, family = gaussian(), adapt_delta = 0.99)

# Trace plot
plot(my_GLM_H, plotfun = "trace", pars = c("(Intercept)", "AGEP", "sigma"), inc_warmup = FALSE)

# Estimate plot
plot(my_GLM_H)

# Leave one out cross-validation
linkGLM_H <- rstanarm::log_lik(my_GLM_H)
loo_GLM_H <- rstanarm::loo(linkGLM_H)
kplot(loo_GLM_H)


my_sexpipe <- stan_glmer(mean_total_earning ~ AGEP + (SEX|Major_Category), data = EHB_subset_filt, family = gaussian(), adapt_delta = 0.99)
post_sexpipe <- posterior_predict(my_sexpipe, data.frame(AGEP = c(30, 40, 30, 40), SEX = c("1", "1", "2", "2"), Major_Category = rep("Business", 4)))



### ----------------------------------------------------------------------------------------- ###


# Generalized linear model with ages only in the interval NO link but as covariate to added to model
my_GLM <- stan_glm(mean_total_earning ~ SEX + AGEP + Major_Category, data = EHB_subset_filt, family = gaussian("identity"), adapt_delta = 0.99)

# Trace plot
plot(my_GLM, plotfun = "trace", pars = c("(Intercept)",  "AGEP", "Major_CategoryEducation", "Major_CategoryHealth", "sigma"), inc_warmup = FALSE)

# Estimate plot
plot(my_GLM)

# Leave one out cross-validation
linkGLM <- rstanarm::log_lik(my_GLM)
loo_GLM <- rstanarm::loo(linkGLM)
kplot(loo_GLM)
