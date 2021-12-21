library(tidyverse)
library(viridis)
library(plotly)

curate_searchable_debate_titles_data <- function(hansard, export_dir, visualize) {
  
  hansard <- hansard %>%
    drop_na() %>%
    rename(year_count = n_debates_w_shared_title)
  
  all_debate_titles_metadata <- hansard %>%
    select(debate, year, year_count)
  write_csv(all_debate_titles_metadata, paste0(export_dir, "all_debate_titles_metadata.csv"))
  
  debate_title_year_counts <- hansard %>%
    select(debate, year, year_count) %>%
    rename(n = year_count)
  write_csv(debate_title_year_counts, paste0(dir, "debate_title_year_counts.csv")) # call instead: debate title counts by year 
  
  keywords_property <- c("croft", "ejectment", "enclosure", "estate", "evict", "inclosure", "landlord", "landown", "property", " rent", "tenant", "tenure")
  keywords_resources <- c("manufacturing", "sugar", "drugs", "water", "agriculture", "markets", "cotton", "corn", "mining", "shop", "farm", "fishery")
  keywords_industry <- c("overcrowding", "pollution", "construction", "dockyards", "harbours", "docks", "oil", "trains", "steamships", "smoke", "industry", "industries")
  keywords_transportation <- c("road", "transport", "railway", "rail", "ship", "steamship", "freights", "cargoes", "wrecks", "cars", "locomotives", "highways")
  keywords_geography <- c("africa", "ireland", "britain", "scotland", "india", "france", "")
  
  subjects <- c("property", "resources", "industry", "transportation", "geography") # automate this ? 
  
  for (subject in subjects) {
    
    s <- ls(pattern = paste0("keywords_", subject))
    
    all_year_counts <- data.frame()
    metadata <- data.frame()
    
    for (keyword in 1:length(keywords)) {
      
      debate_titles_w_keyword <- hansard %>%
        filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b|^", keyword, "\\b|\\b", keyword, "$"), ignore_case = TRUE)))
      
      current_metadata <- debate_titles_w_keyword %>%
        select(debate, year, year_count) %>%
        mutate(keywords = keyword) # change to keyword = keyword once i am ready to adjust the rest of the code 
      
      metadata <- bind_rows(metadata, current_metadata)
      
      year_count <- debate_titles_w_keyword %>%
        group_by(year) %>%
        summarise(debates_per_year = sum(year_count)) %>% 
        mutate(keywords = keyword) # change to keyword = keyword once i am ready to adjust the rest of the code 
      
      all_year_counts <- rbind(all_year_counts, year_count) }
    
    write_csv(all_year_counts, paste0(export_dir, "all_year_counts_", subject, ".csv"))
    write_csv(metadata, paste0(export_dir, "kw_metadata_", subject, ".csv"))
    
    words_per_year <- hansard %>%
      group_by(year) %>%
      summarise(words_per_year = sum(year_count)) 
    write_csv(words_per_year, paste0(export_dir, "words_per_year.csv"))
    
    all_year_counts <- all_year_counts %>%
      left_join(words_per_year, by = "year") %>%
      mutate(proportion = debates_per_year/words_per_year)
    write_csv(all_year_counts, paste0(export_dir, "kw_list_", subject, ".csv"))
    
    if (visualize == TRUE) {
      
      plot <- ggplot(data = all_year_counts) +
        geom_col(aes(x = year, 
                     y = proportion,
                     fill=reorder(keywords, debates_per_year))) + 
        scale_fill_viridis(discrete = TRUE, option = "C")+
        guides(fill = guide_legend(title = "Keywords")) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        labs(y = "debates per year as proportion", 
             title = "Count of Debate Titles That Include Keywords")
      
      ggplotly(plot)
      
      } } }

