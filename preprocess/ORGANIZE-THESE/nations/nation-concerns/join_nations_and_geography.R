library(tidyverse)

nations_concerns_collocates <- read_csv("~/nations_concerns_09052021.csv")

collaborative_nations<- read_csv("~/collaborative_nations.csv") %>%
  select(Nation_Base) %>%
  rename(nation = Nation_Base)
  
geography <- read_csv("~/collaborative_nations.csv") %>%
  select(Geography) %>%
  rename(geograpthy = Geography)

collaborative_nations$sentence_id <- seq.int(nrow(collaborative_nations))
geography$sentence_id <- seq.int(nrow(geography))

collaborative_nations <- collaborative_nations %>%
  distinct(nation, .keep_all= T) %>%
  drop_na()

cleaned <- left_join(collaborative_nations, geography, by = "sentence_id")


out <- left_join(nations_concerns_collocates, cleaned, by = "nation")

