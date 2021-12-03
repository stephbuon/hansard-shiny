library(tidyverse)

export_unique_debate_titles <- function (df) {

  df <- df %>%
  select(debate_id, debate, year)

  df$debate <- str_to_title(df$debate)
  
  df <- df %>%
    count(debate_id, debate, year, decade) %>%
    rename(unique_debate_year_count = n)
  
  df <- df %>%
    group_by(debate, year) %>%
    summarise(n_debates_w_shared_title = sum(unique_debate_year_count))
  
  df <- df %>%
    drop_na(debate) 
  
  return (df) }
  
  #write_csv(b, "~/hansard_c19_debate_titles_w_year.csv") }

