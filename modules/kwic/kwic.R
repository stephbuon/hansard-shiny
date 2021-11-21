library(data.table)
library(stringr)

txt <- fread("/home/stephbuon/projects/hansard-shiny/hansard_decades_text2vec1803_1807.csv")

str_extract_all(txt,"([^\\s]+\\s+){0,5}Jane(\\s+[^\\s]+){0,5}")


# library(quanteda)
# library(readtext)

# h <- readtext("/home/stephbuon/projects/hansard-shiny/hansard_decades_text2vec1803_1807.csv")
# 
# h <- corpus(h)
# 
# # regex or fixed
# # if regex, remember I can include *
# 
# 
# 
# t <- kwic(h, "government", window = 10, valuetype = "fixed", separator = " ", case_insensitive = TRUE)
# t <- kwic(h, phrase = ("united states"), window = 10, valuetype = "fixed", separator = " ", case_insensitive = TRUE)
