library(data.table)
library(dplyr)
library(readr)
library(stringr)

dir <- "/scratch/group/pract-txt-mine/sbuongiorno/"

original_nations <- read_csv("~/collaborative_nations.csv")
nations <- read_csv("~/nations.csv")
nations <- nations$nations

decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900)
full_hansard <- fread("/scratch/group/pract-txt-mine/sbuongiorno/hansard_c19_improved_speaker_names_2.csv") %>%
  select(year, text)

full_hansard <- full_hansard %>%
  mutate(decade = year - year %% 10)

full_hansard <- full_hansard %>%
  select(decade, text)

counted_nations_w_decade <- data.frame()
for (d in decades) {

  hansard <- full_hansard %>%
    filter(decade == d)

  counted_nations <- data.frame()
  for (n in nations) {
    counts <- hansard %>%
      mutate(nation_counts_per_row = str_count(text, regex(paste0("\\b", n, "\\b"), ignore_case = T)))

    counts <- counts %>%
      summarise(nation_count = sum(nation_counts_per_row))

    counts <- counts %>%
      mutate(nation = n) %>%
      mutate(decade = d)

    counted_nations <- bind_rows(counted_nations, counts) }

  counted_nations_w_decade <- bind_rows(counted_nations_w_decade, counted_nations) }

args_1 <- original_nations$Nations
args_2 <- original_nations$Nation_Base

str_repare <- function(vctr, args_1, args_2){
  for(i in seq_along(args_1)){
    vctr <- str_replace_all(vctr, paste0("^", args_1[[i]], "$"), args_2[[i]])
  }
  return(vctr)
}

counted_nations_w_decade$nation <- str_repare(counted_nations_w_decade$nation, args_1, args_2)

counted_nations_w_decade <- read_csv("/home/stephbuon/projects/hansard-shiny/CATEGORIZE/hansard_nation_counts_check.csv")

a$nation <- a$nation %>%
  str_to_title()


z <- counted_nations_w_decade %>%
  group_by(nation, decade) %>%
  summarise(nation_count = sum(nation_count))

z$nation <- str_replace_all(z$nation, "_", " ")


fwrite(z, paste0(dir, "hansard_nation_counts.csv"))
