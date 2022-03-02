library(tidyverse)
library(tidytext)


str_repare <- function(vctr, args_1, args_2) {
  for(i in seq_along(args_1)){
    vctr <- str_replace_all(vctr, paste0("^", args_1[[i]], "$"), args_2[[i]])
  }  
  
  return(vctr) }


count_nations_in_debate_titles <- function(hansard, nations) {
  
  hansard <- hansard %>%
    select(sentence_id, debate, year)
  
  nations_list <- nations$nations
  
  output <- data.frame()
  
  for(i in 1:length(nations_list)) {
    nation <- nations_list[i]
    
    print(paste0("Working on ", nation))
    
    filtered_hansard <- hansard %>%
      filter(str_detect(debate, regex(nation, ignore_case = TRUE)))
    
    filtered_hansard$debate <- nation # to make names consistent
    
    output <- bind_rows(output, filtered_hansard) } 
  
  
  
  original_nations <- read_csv(paste0(preprocess_data_dir, "origin-data/keywords-list/collaborative_nations.csv"))
  
  args_1 <- original_nations$Nations
  args_2 <- original_nations$Nation_Base
  
  output$debate <- str_repare(output$debate, args_1, args_2)
  
  
  output_2 <- output %>%
    count(debate, decade)
  
  output_2 <- output_2 %>%
    filter(!str_detect(debate, "^Africa|Europe|World"))
  
  output_2$debate <- str_replace_all(output_2$debate, "_", " ")
  
  
  write_csv(output, "~/hansard_c19_debate_title_nation_count.csv") }

count_nations_in_debate_titles("hansard_tokens_c19_improved_speaker_names_app_data.csv", "nations.csv")
