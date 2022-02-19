text2vec_decade_subsets <- function(hansard, export_dir) {
  
  decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900)
  decades2 <- c(09, 19, 29, 39, 49, 59, 69, 79, 89, 99, 09)
  
  hansard <- hansard %>%
    select(year, ngram) %>%
    mutate(decade = year - year %% 10) %>%

  for(d in decades) {
    
    hansard <- hansard %>%
      filter(decade == d) %>%
      select(-decade)
    
    for(d2 in decades2) {
      
      fwrite(hansard, paste0(export_dir, "hansard_decades_text2vec_", d, "_", d2, ".csv")) } } }
    
    
