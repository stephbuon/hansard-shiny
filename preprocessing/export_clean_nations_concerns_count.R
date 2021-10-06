library(tidyverse)

a <- read_csv("~/nation_concern_count_1800.txt", col_names = F) %>%
  mutate(decade = 1800)
b <- read_csv("~/nation_concern_count_1810.txt", col_names = F) %>%
  mutate(decade = 1810)
c <- read_csv("~/nation_concern_count_1820.txt", col_names = F) %>%
  mutate(decade = 1820)
d <- read_csv("~/nation_concern_count_1830.txt", col_names = F) %>%
  mutate(decade = 1830)
e <- read_csv("~/nation_concern_count_1840.txt", col_names = F) %>%
  mutate(decade = 1840)
f <- read_csv("~/nation_concern_count_1850.txt", col_names = F) %>%
  mutate(decade = 1850)
g <- read_csv("~/nation_concern_count_1860.txt", col_names = F) %>%
  mutate(decade = 1860)
h <- read_csv("~/nation_concern_count_1870.txt", col_names = F) %>%
  mutate(decade = 1870)
i <- read_csv("~/nation_concern_count_1880.txt", col_names = F) %>%
  mutate(decade = 1880)
j <- read_csv("~/nation_concern_count_1890.txt", col_names = F) %>%
  mutate(decade = 1890)
k <- read_csv("~/nation_concern_count_1900.txt", col_names = F) %>%
  mutate(decade = 1900)
l <- read_csv("~/nation_concern_count_1910.txt", col_names = F) %>%
  mutate(decade = 1910)

out <- bind_rows(a, b, c, d, e, f, g, h, i, j, k, l)

out <- out %>%
  rename(nation = X1,
         concern = X2, 
         count = X3)

tt <- out %>%
  count(nation,concern,count,decade) %>%
  select(-count)

tt <- tt %>%
  drop_na()

tt$nation <- str_to_title(tt$nation) 

concerns_fr <- read_csv("~/collaborative_nations.csv")



args_1 <- concerns_fr$Nations
args_2 <- concerns_fr$Nation_Base

str_repare <- function(vctr, args_1, args_2){
  for(i in seq_along(args_1)){
    vctr <- str_replace_all(vctr, paste0("^", args_1[[i]], "$"), args_2[[i]])
  }  
  return(vctr)
}

tt$nation <- str_repare(tt$nation, args_1, args_2)

write_csv(tt, "~/nations_concerns_count_09052021.csv")
