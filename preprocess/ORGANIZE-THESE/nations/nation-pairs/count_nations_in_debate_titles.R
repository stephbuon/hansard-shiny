library(tidyverse)
library(tidytext)

hansard <- read_csv("~/hansard_justnine_w_year.csv") %>%
  select(sentence_id, debate, year)

nations <- read_csv("~/nations.csv")

nations_list <- nations$nations

decade <- 10

hansard <- hansard %>%
  mutate(decade = year - year %% decade) %>%
  select(-year)

output <- data.frame()

for(i in 1:length(nations_list)) {
  nation <- nations_list[i]
    
  print(paste0("Working on ", nation))
    
  filtered_hansard <- hansard %>%
    filter(str_detect(debate, regex(nation, ignore_case = TRUE)))
    
  filtered_hansard$debate <- nation # to make names consistent
    
  output <- bind_rows(output, filtered_hansard) } 



original_nations <- read_csv("~/collaborative_nations.csv")

args_1 <- original_nations$Nations
args_2 <- original_nations$Nation_Base

str_repare <- function(vctr, args_1, args_2){
  for(i in seq_along(args_1)){
    vctr <- str_replace_all(vctr, paste0("^", args_1[[i]], "$"), args_2[[i]])
  }  
  return(vctr)
}

output$debate <- str_repare(output$debate, args_1, args_2)


output_2 <- output %>%
  count(debate, decade)

output_2 <- output_2 %>%
  filter(!str_detect(debate, "^Africa|Europe|World"))

output_2$debate <- str_replace_all(output_2$debate, "_", " ")


write_csv(output, "~/hansard_c19_debate_title_nation_count.csv")
