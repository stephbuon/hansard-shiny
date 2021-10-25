library(tidyverse)

wc <- read_csv("~/projects/hansard-shiny/longest_debates_wordcloud.csv")


a <- wc %>%
  group_by(speechdate, debate_id, debate) %>%
  arrange(desc(token_count)) %>%
  slice(1:60)


write_csv(a, "~/projects/hansard-shiny/clean_longest_debates_wordcloud.csv")
