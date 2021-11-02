library(tidyverse)

# for the first time we can do legitimate speaker analysis. 
# what is a way to show off speaker analysis and also triples extaction?

# messy data

hansard_triples <- read_csv("/scratch/group/pract-txt-mine/c19_hansard_debate_text_triples_07232021.csv") %>%
  rename(sentence_id = doc_id) %>%
  select(sentence_id, speechdate, triple, debate, year)

hansard <- read_csv("/scratch/group/pract-txt-mine/hansard_c19_12192019.csv") %>%
  select(sentence_id, speaker)

gladstone <- hansard %>%
  filter(str_detect(speaker, regex("gladstone", ignore_case = TRUE)))

gladstone$speaker <- "Mr. Gladstone"

speaker <- left_join(gladstone, hansard_triples, by = "sentence_id")

speaker <- speaker %>%
  drop_na(triple)

write_csv(speaker, "~/gladstone_triples.csv")
