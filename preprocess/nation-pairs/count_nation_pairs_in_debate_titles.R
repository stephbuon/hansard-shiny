library(tidyverse)


clean_nation_pairs_in_debate_titles <- function(nations) {
  
  
  #nations <- read_csv("~/projects/hansard-shiny/data/nations/nation_pairs_in_titles.csv") %>%
  #  select(-Nations)
  
  nations <- nations %>%
    select(-Nations)
  
  nations <- nations %>%
    drop_na(Nation1, Nation2)
  
  decade <- 10
  
  nations <- nations %>%
    mutate(decade = Year - Year %% decade)
  
  nations <- nations %>%
    group_by(decade, Nation1, Nation2) %>%
    summarize(n = sum(Frequency)) %>%
    ungroup()
  
  nations$nation_pair <- paste0(nations$Nation1, '-', nations$Nation2)
  
 return(nations)
  
}

str_repare <- function(vctr, args_1, args_2){
  for(i in seq_along(args_1)){
    vctr <- str_replace_all(vctr, paste0("^", args_1[[i]], "$"), args_2[[i]])
  }  
  return(vctr)
}

count_nation_pairs_in_debate_titles <- function() {
  
  decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900)
  
  out <- data.frame()
  
  for(d in decades) {
    
    current_hansard <- fread(paste0("nation_concern_count_", d, ".csv", col_names = F)) #%>%
    #mutate(decade = d)
    
    out <- bind_rows(out, current_hansard)
    
  }
  
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
  
  
  out$nation1 <- str_repare(out$nation1, args_1, args_2)
  out$nation2 <- str_repare(out$nation2, args_1, args_2)
  
  
  out <- out %>%
    count(nation1, nation2, decade) 
  
  out$nation_pair <- paste0(out$nation1, "-", out$nation2)
  
  out$nation_pair <- str_to_title(out$nation_pair) 
  
  out$nation_pair <- str_replace_all(out$nation_pair, "_", " ")
  
  #write_csv(out, "~/nation_pairs_in_debate_titles.csv")
  
  
  n <- clean_nation_pairs_in_debate_titles(out)
  
  write_csv(n, paste0(n, "~/projects/hansard-shiny/data/nations/clean_nation_pairs_in_titles.csv"))
  
}


count_nation_pairs_in_debate_titles()
