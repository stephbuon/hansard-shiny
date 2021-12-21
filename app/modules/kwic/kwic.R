library(data.table)
library(stringr)
library(quanteda)
library(dplyr)

txt <- fread("/home/stephbuon/Downloads/hansard_justnine_w_year.csv")

txt <- txt %>%
  select(sentence_id, year, debate, text, speaker)

txt <- txt %>%
  mutate(decade = year - year %% 10)

txt <- txt %>%
  select(-year)

b <- txt %>%
  filter(decade == 1830)



h <- corpus(b)
# 
# # regex or fixed
# # if regex, remember I can include *
# 
# 
# 
 t <- kwic(h, "government", window = 10, valuetype = "fixed", separator = " ", case_insensitive = TRUE)
# t <- kwic(h, phrase = ("united states"), window = 10, valuetype = "fixed", separator = " ", case_insensitive = TRUE)
