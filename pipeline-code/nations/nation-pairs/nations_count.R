library(tidyverse)

nations <- read_csv("~/projects/hansard-shiny/data/nations/nation_pairs_in_titles.csv") %>%
  select(-Nations)

nations <- nations %>%
  drop_na(Nation1, Nation2)

decade <- 10

nations <- nations %>%
  mutate(decade = Year - Year %% decade)

nations <- nations %>%
  group_by(decade, Nation1, Nation2) %>%
  summarize(n = sum(Frequency)) %>%
  ungroup()

nations$nation_pair <- paste0(nations$Nation1, '-', nations$Nation2)

write_csv(nations, "~/projects/hansard-shiny/data/nations/clean_nation_pairs_in_titles.csv")
