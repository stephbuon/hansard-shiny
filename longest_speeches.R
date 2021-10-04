library(tidyverse)

longest_speeches <- read_csv("~/projects/hansard-shiny/data/speakers/longest_speeches.csv")

longest_speeches$debate <- gsub("\\*", "", longest_speeches$debate)
longest_speeches$debate <- str_replace_all(longest_speeches$debate, "\\[", "")
longest_speeches$debate <- str_replace_all(longest_speeches$debate, "\\]", "")
longest_speeches$debate <- str_replace_all(longest_speeches$debate, "\\(", "")
longest_speeches$debate <- str_replace_all(longest_speeches$debate, "\\)", "")
longest_speeches$debate <- str_replace_all(longest_speeches$debate, "\\.", "")
longest_speeches$debate <- str_replace_all(longest_speeches$debate, "\\,", "")

longest_speeches <- longest_speeches %>%
  rename(decade = `10 * floor(year/10)`)

write_csv(longest_speeches, "~/longest_speeches.csv")
