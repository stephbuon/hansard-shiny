subset_hansard_tokens <- function(hansard, preproces_data_dir) {
  
  decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900)
  
  for(d in decades) {
    
    hansard_decade <- hansard %>%
      filter(decade = d)
    
    fwrite(hansard_decade, paste0(preproces_data_dir, "hansard_tokens_subset_", d, ".csv")) } }

