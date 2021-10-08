library(tidyverse)

a <- read_csv("~/nation_concern_count_1800.csv", col_names = F) %>%
  mutate(decade = 1800)
b <- read_csv("~/nation_concern_count_1810.csv", col_names = F) %>%
  mutate(decade = 1810)
c <- read_csv("~/nation_concern_count_1820.csv", col_names = F) %>%
  mutate(decade = 1820)
d <- read_csv("~/nation_concern_count_1830.csv", col_names = F) %>%
  mutate(decade = 1830)
e <- read_csv("~/nation_concern_count_1840.csv", col_names = F) %>%
  mutate(decade = 1840)
f <- read_csv("~/nation_concern_count_1850.csv", col_names = F) %>%
  mutate(decade = 1850)
g <- read_csv("~/nation_concern_count_1860.csv", col_names = F) %>%
  mutate(decade = 1860)
h <- read_csv("~/nation_concern_count_1870.csv", col_names = F) %>%
  mutate(decade = 1870)
i <- read_csv("~/nation_concern_count_1880.csv", col_names = F) %>%
  mutate(decade = 1880)
j <- read_csv("~/nation_concern_count_1890.csv", col_names = F) %>%
  mutate(decade = 1890)
k <- read_csv("~/nation_concern_count_1900.csv", col_names = F) %>%
  mutate(decade = 1900)
l <- read_csv("~/nation_concern_count_1910.csv", col_names = F) %>%
  mutate(decade = 1910)

out <- bind_rows(a, b, c, d, e, f, g, h, i, j, k, l)


out <- out %>%
  rename(nation1 = X1,
         nation2 = X2, 
         debate_id = X4) %>%
  select(-X3)

out <- out[ !duplicated(apply(out, 1, sort), MARGIN = 2), ] # remove duplicates regardless of order


name_correction <- read_csv("~/collaborative_nations.csv")

args_1 <- name_correction$Nations
args_2 <- name_correction$Nation_Base

args_1 <- tolower(args_1)
args_2 <- tolower(args_2)

str_repare <- function(vctr, args_1, args_2){
  for(i in seq_along(args_1)){
    vctr <- str_replace_all(vctr, paste0("^", args_1[[i]], "$"), args_2[[i]])
  }  
  return(vctr)
}

out$nation1 <- str_repare(out$nation1, args_1, args_2)
out$nation2 <- str_repare(out$nation2, args_1, args_2)


out <- out %>%
  count(nation1, nation2, decade) 

out$nation_pair <- paste0(out$nation1, "-", out$nation2)

out$nation_pair <- str_to_title(out$nation_pair) 

out$nation_pair <- str_replace_all(out$nation_pair, "_", " ")

write_csv(out, "~/nation_pairs_in_debate_titles.csv")
