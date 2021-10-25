library(tidyverse)

hansard <- read_csv("~/hansard_justnine_w_year.csv")

decade <- 10 

hansard <- hansard %>% 
  select(debate_id, debate, year)

hansard$debate <- gsub("\\*", "", hansard$debate)
hansard$debate <- str_replace_all(hansard$debate, "\\,", "")
hansard$debate <- str_replace_all(hansard$debate, "\\[", "")
hansard$debate <- str_replace_all(hansard$debate, "\\]", "")
hansard$debate <- str_replace_all(hansard$debate, "\\(", "")
hansard$debate <- str_replace_all(hansard$debate, "\\)", "")
hansard$debate <- str_replace_all(hansard$debate, "\\.$", "")
hansard$debate <- str_replace_all(hansard$debate, "\\—", "")

hansard <- hansard %>%
  mutate(decade = year - year %% decade) %>%
  count(debate_id, debate, year, decade) %>%
  rename(unique_debate_year_count = n)

hansard <- hansard %>%
  group_by(debate, year) %>%
  summarise(n_debates_w_shared_title = sum(unique_debate_year_count))

  

write_csv(hansard, "~/hansard_c19_debate_titles_w_year.csv")





####### new version: 

library(tidyverse)

hansard <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/hansard_data/hansard_justnine_w_year.csv")

decade <- 10 

hansard <- hansard %>% 
  select(debate_id, debate, year)

hansard$debate <- gsub("\\*", "", hansard$debate)
hansard$debate <- str_replace_all(hansard$debate, "\\,", "")
hansard$debate <- str_replace_all(hansard$debate, "\\[", "")
hansard$debate <- str_replace_all(hansard$debate, "\\]", "")
hansard$debate <- str_replace_all(hansard$debate, "\\(", "")
hansard$debate <- str_replace_all(hansard$debate, "\\)", "")
hansard$debate <- str_replace_all(hansard$debate, "\\.", "")
hansard$debate <- str_replace_all(hansard$debate, "\\—", "")
hansard$debate <- str_replace_all(hansard$debate, "\"", "")
hansard$debate <- str_replace_all(hansard$debate, "\'", "")
hansard$debate <- str_replace_all(hansard$debate, "\\>", "")
hansard$debate <- str_replace_all(hansard$debate, "^ ", "")


hansard$debate <- str_to_title(hansard$debate)

a <- hansard %>%
  mutate(decade = year - year %% decade) %>%
  count(debate_id, debate, year, decade) %>%
  rename(unique_debate_year_count = n)

b <- a %>%
  group_by(debate, year) %>%
  summarise(n_debates_w_shared_title = sum(unique_debate_year_count))

b <- b %>%
  drop_na(debate)


write_csv(b, "~/hansard_c19_debate_titles_w_year.csv")


