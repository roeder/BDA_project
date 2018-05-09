library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# us16_filtered <- read_csv("data/us16_filtered.csv")
# 
# # download from https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/majors-list.csv
# majors_list <- read_csv("data/majors-list.csv") %>% 
#   filter(! FOD1P == "bbbb") %>% 
#   mutate(FOD1P = as.integer(FOD1P))
# 
# # Add major metadata, then convert total earnings to inflation-adjusted earnings
# us16_filtered <- us16_filtered %>% 
#   left_join(majors_list) %>% 
#   mutate(PERNP = as.integer(PERNP),
#          WAGP = as.integer(WAGP),
#          real_total_earn = PERNP * (ADJINC / 1000000),
#          real_wage = WAGP * (ADJINC / 1000000))
# 
# selected_vars <- us16_filtered %>% 
#   select(SERIALNO, ST, AGEP, SEX, FOD1P, Major, Major_Category, real_total_earn, real_wage)
# 
# write_csv(selected_vars, "data/selected_vars.csv")

selected_vars <- read_csv("data/selected_vars.csv")

state_co_df <- selected_vars %>% 
  group_by(ST) %>% 
  summarise(entries = n())

n_records_model <- selected_vars %>% 
  filter(ST %in% c(36, 48),
         ! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 50) %>% 
  group_by(Major_Category, SEX, ST) %>% 
  summarise(n_entries = n()) %>% 
  ungroup() %>% 
  mutate(sex = if_else(SEX == 1, "male", "female"),
         state = if_else(ST == 36, "NY", "TX")) %>% 
  select(state, sex, Major_Category, n_entries) %>% 
  spread(., key = sex, value = n_entries)

n_records_tx <- n_records_model %>% 
  filter(state == "TX") %>% 
  rename(tx_female = female,
         tx_male = male) %>% 
  select(- state)
  
combined_n_records <- n_records_model %>% 
  filter(state == "NY") %>% 
  rename(ny_female = female,
         ny_male = male) %>% 
  select(- state) %>% 
  left_join(n_records_tx)

medians_df <- selected_vars %>% 
  filter(ST %in% c(36, 48)) %>% 
  group_by(Major_Category, AGEP, SEX, ST) %>% 
  summarise(median_total_earning = median(real_total_earn, na.rm = T),
            median_wage = median(real_wage, na.rm = T),
            entries = n()) %>% 
  filter(! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 50)

norm_dist_df <- selected_vars %>% 
  filter(ST %in% c(36, 48)) %>% 
  group_by(Major_Category, AGEP, SEX) %>% 
  summarise(mean_total_earning = mean(real_total_earn, na.rm = T),
            sd_total_earning = sd(real_total_earn, na.rm = T),
            entries = n()) %>% 
  ungroup() %>% 
  mutate(sem = sd_total_earning / sqrt(entries)) %>% 
  filter(! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 70)

norm_dist_df %>% 
  ggplot(., aes(x = AGEP)) +
  geom_ribbon(data = filter(norm_dist_df, SEX == 1),
              aes(ymin = mean_total_earning - 1.96 * sem,
                  ymax = mean_total_earning + 1.96 * sem),
              alpha = 0.2) +
  geom_ribbon(data = filter(norm_dist_df, SEX == 2),
              aes(ymin = mean_total_earning - 1.96 * sem,
                  ymax = mean_total_earning + 1.96 * sem),
              alpha = 0.2) +
  geom_line(aes(y = mean_total_earning, colour = factor(SEX))) +
  # geom_line(aes(y = mean_total_earning + 1.96 * sem, 
  #               colour = factor(SEX)), linetype = 2) +
  # geom_line(aes(y = mean_total_earning - 1.96 * sem, 
  #               colour = factor(SEX)), linetype = 2) +
  scale_colour_discrete(name = "sex", labels = c("male", "female")) +
  facet_wrap(~ Major_Category) +
  theme(legend.position = c(.95, 0), legend.justification = c(1, 0))


medians_df %>% 
  filter(! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 70) %>% 
  ggplot(., aes(x = AGEP)) +
  ggtitle("Median total yearly earnings in different degree categories") +
  ylab("median total earnings in $") +
  xlab("age") +
  geom_line(aes(y = median_total_earning, colour = factor(SEX))) +
  geom_line(aes(y = median_wage, colour = factor(SEX)), linetype = 2) +
  scale_colour_discrete(name = "sex", labels = c("male", "female")) +
  facet_wrap(~ Major_Category) +
  theme(legend.position = c(.95, 0), legend.justification = c(1, 0))

medians_df %>% 
  filter(! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 70) %>% 
  ggplot(., aes(x = AGEP, y = median_total_earning)) +
  ggtitle("Median total yearly earnings in different degree categories") +
  ylab("median total earnings in $") +
  xlab("age") +
  geom_line(aes(colour = factor(SEX))) +
  scale_colour_discrete(name = "sex", labels = c("male", "female")) +
  facet_wrap(~ Major_Category) +
  theme(legend.position = c(.95, 0), legend.justification = c(1, 0))

medians_df %>% 
  filter(! Major_Category == "Interdisciplinary",
         AGEP >= 25,
         AGEP <= 70) %>% 
  ggplot(., aes(x = AGEP, y = entries)) +
  ggtitle("Number of individual records in different degree categories") +
  ylab("no. of records") +
  xlab("age") +
  geom_line(aes(colour = factor(SEX))) +
  # geom_bar(position = "dodge", stat = "identity", aes(fill = factor(SEX))) +
  # scale_fill_discrete(name = "sex", labels = c("male", "female")) +
  scale_colour_discrete(name = "sex", labels = c("male", "female")) +
  facet_wrap(~ Major_Category) +
  theme(legend.position = c(.95, 0), legend.justification = c(1, 0))
