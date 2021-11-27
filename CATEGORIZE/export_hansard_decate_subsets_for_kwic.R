library(data.table)
library(dplyr)

hansard_debates <- fread("/home/stephbuon/Downloads/hansard_justnine_w_year.csv")

hansard_debates <- hansard_debates %>%
  select(sentence_id, year, text) %>%
  #select(sentence_id, year, debate, text, speaker)%>%
  mutate(decade = year - year %% 10) %>%
  select(-year)

d <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900)

for (decade in decades) {
  
  hansard_debates_subset <- hansard_debates %>%
    filter(decade == d) %>%
    select(-decade)

  dir <- "/home/stephbuon/projects/hansard-shiny/app-data/kwic/"
  fwrite(hansard_debates_subset, paste0(dir, "hansard_", decade, ".csv")) }

