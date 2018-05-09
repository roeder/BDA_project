library(readr) # faster file reading & writing, with progress bars
library(dplyr) # fast filtering

us_a_df <- read_csv("raw_data/csv_pus/ss16pusa.csv")

a_filtered <- us_a_df %>% 
  filter(! is.na(FOD1P))

nrow(a_filtered) / nrow(us_a_df)
# [1] 0.2312608
rm(us_a_df)

us_b_df <- read_csv("raw_data/csv_pus/ss16pusb.csv")

b_filtered <- us_b_df %>% 
  filter(! is.na(FOD1P))

nrow(b_filtered) / nrow(us_b_df)
# [1] 0.216856
rm(us_b_df)

us_c_df <- read_csv("raw_data/csv_pus/ss16pusc.csv")

c_filtered <- us_c_df %>% 
  filter(! is.na(FOD1P))

nrow(c_filtered) / nrow(us_c_df)
# [1] 0.2259818
rm(us_c_df)

us_d_df <- read_csv("raw_data/csv_pus/ss16pusd.csv")

d_filtered <- us_d_df %>% 
  filter(! is.na(FOD1P))

nrow(d_filtered) / nrow(us_d_df)
# [1] 0.2137309
rm(us_d_df)

a_filtered$ST <- as.integer(gsub("(?<![0-9])0+", "", a_filtered$ST , perl = TRUE))

all_filtered <- bind_rows(a_filtered, b_filtered, c_filtered, d_filtered)

write_csv(all_filtered, "data/us16_filtered.csv")
