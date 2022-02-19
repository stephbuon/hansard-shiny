export_unique_debate_titles <- function (hansard) {

  hansard <- hansard %>%
    select(debate_id, debate, year)

  hansard$debate <- str_to_title(hansard$debate)
  
  hansard <- hansard %>%
    count(debate_id, debate, year, decade) %>%
    rename(unique_debate_year_count = n) %>%
    group_by(debate, year) %>%
    summarise(n_debates_w_shared_title = sum(unique_debate_year_count))
  
  hansard <- hansard %>%
    drop_na(debate) 
  
  return (hansard) }
  

