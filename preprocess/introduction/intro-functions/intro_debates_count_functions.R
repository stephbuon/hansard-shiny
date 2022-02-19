intro_viz <- function(root_dir, app_data_dir, hansard) {
  
  hansard <- hansard %>%
    select(debate_id, decade) %>%
    distinct(debate_id, decade) %>%
    group_by(decade) %>%
    summarize(no_of_debates = n())
  
  return(hansard) }
