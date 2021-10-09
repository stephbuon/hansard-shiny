library(tidyverse)

hansard <- read_csv("~/hansard_justnine_w_year.csv")

decade <- 10 

hansard <- hansard %>%
  select(debate_id, debate, year) %>%
  mutate(decade = year - year %% decade) %>%
  count(debate_id, debate, year, decade) %>%
  rename(group_count = n)
  

write_csv(h, "~/hansard_c19_debate_titles_w_year.csv")
