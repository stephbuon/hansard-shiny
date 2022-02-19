kwic_decade_subsets <- function(hansard, export_dir) {
  
  d <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900)
  
  hansard <- hansard %>%
    select(sentence_id, year, text) %>%
    #select(sentence_id, year, debate, text, speaker)%>%
    mutate(decade = year - year %% 10) %>%
    select(-year)
  
  for (decade in d) {
    
    hansard <- hansard %>%
      filter(decade == d) %>%
      select(-decade)
    
    fwrite(hansard, paste0(export_dir, "hansard_", decade, ".csv")) } }

