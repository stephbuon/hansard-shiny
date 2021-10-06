library(tidyverse)


nations <- read_csv("~/collaborative_nations.csv")

nations <- unique(nations)

nations <- nations %>%
filter(!str_detect(nations, "\\'s')) # get rid of possessive nations

write_csv(nations, "~/nations.csv")
