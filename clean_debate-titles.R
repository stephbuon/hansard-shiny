library(tidyverse)

edges <- read_csv("hansard_justnine_debate_titles_w_year.csv")

edges$debate <- str_replace_all(edges$debate, "\\[", "")
edges$debate <- str_replace_all(edges$debate, "\\]", "")
edges$debate <- str_replace_all(edges$debate, "\\(", "")
edges$debate <- str_replace_all(edges$debate, "\\)", "")
edges$debate <- str_replace_all(edges$debate, "\\)", "")
edges$debate <- str_replace_all(edges$debate, "\\—", "")
edges$debate <- str_replace_all(edges$debate, "\\.", "")
edges$debate <- str_replace_all(edges$debate, "\\*", "")
edges$debate <- str_replace_all(edges$debate, "\"", "")

edges <- edges %>%
  group_by(debate, decade) %>%
  add_count() %>%
  rename(group_count = n) %>%
  drop_na()

edges <- unique(edges)

edges <- edges %>% 
  filter(!str_detect(debate, "BILL (.*) SECOND READING")) %>%
  filter(!str_detect(debate, "^SECOND READING$")) %>%
  filter(!str_detect(debate, "^RESOLUTION$")) %>%
  filter(!str_detect(debate, "^COMMITTEE$")) %>%
  filter(!str_detect(debate, "^OBSERVATIONS$")) %>%
  filter(!str_detect(debate, "^QUESTION$")) %>%
  filter(!str_detect(debate, "^CLASS II$")) %>%
  filter(!str_detect(debate, "^SUPPLYNAVY ESTIMATES$")) 


edges <- edges %>% 
  filter(!str_detect(debate, "^BILL PRESENTED FIRST READING$")) %>%
  filter(!str_detect(debate, "(.*) FIRST READING")) %>%
  filter(!str_detect(debate, "(.*) LEAVE FIRST READING"))

write_csv(edges, "hansard_justnine_debate_titles_w_year.csv")
