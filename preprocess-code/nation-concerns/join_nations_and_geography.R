library(tidyverse)

append_nation_concerns_count_with_geography <- function(nation_concerns_data, nations_data) {
  
  nations_concerns_collocates <- fread(nation_concerns_data)
  
  collaborative_nations<- fread(nations_data) %>%
    select(Nation_Base) %>%
    rename(nation = Nation_Base)
  
  geography <- read_csv(nations_data) %>%
    select(Geography) %>%
    rename(geograpthy = Geography)
  
  collaborative_nations$sentence_id <- seq.int(nrow(collaborative_nations))
  geography$sentence_id <- seq.int(nrow(geography))
  
  collaborative_nations <- collaborative_nations %>%
    distinct(nation, .keep_all= T) %>%
    drop_na()
  
  cleaned <- left_join(collaborative_nations, geography, by = "sentence_id")
  
  out <- left_join(nations_concerns_collocates, cleaned, by = "nation")
  
  fwrite(out, paste0(preprocess_data_dir, "nation_concerns_count.csv"))
  
  
}

append_nation_concerns_count_with_geography("~/nations_concerns_09052021.csv", "~/collaborative_nations.csv")


