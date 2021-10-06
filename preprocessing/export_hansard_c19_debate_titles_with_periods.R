library(tidyverse)

hansard <- read_csv("~/hansard_justnine_w_year.csv")

decade <- 10

hansard <- hansard %>%
  mutate(decade = year - year %% decade, text)

hansard <- hansard %>%
  select(debate, year, decade)

write_csv(hansard, "hansard_c19_debate_titles_w_year.csv")
