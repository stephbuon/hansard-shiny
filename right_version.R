h <- read_csv("~/hansard_justnine_w_year.csv")

h <- h %>%
  select(debate_id, debate, year)

decade <- 10 

h <- h %>%
  mutate(decade = year - year %% decade)

h <- h %>%
  count(debate_id, debate, year, decade) %>%
  rename(group_count = n)
  

write_csv(h, "~/hansard_c19_debate_titles_w_year.csv")
