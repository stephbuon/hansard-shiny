keyword_addition <- function(d,words_per_year, debate_title_year_counts, input_kw_list, input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6){
  
  keywords <- stri_remove_empty(c(input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6), TRUE)
  
  if (input_kw_list != "custom") { 
    all_year_counts <- fread(paste0(data_dir, "debates/all_year_counts_", input_kw_list, ".csv")) } 
  else {
    all_year_counts <- fread(paste0(data_dir, "debates/all_debate_titles_metadata.csv")) }
  
  if (length(keywords != 0)) {
    for (keyword in keywords) {
      if (!keyword %in% all_year_counts$keywords) {
        
        debate_titles_w_keyword <- debate_title_year_counts %>%
          filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b|^", keyword, "\\b|\\b", keyword, "$"), ignore_case = TRUE)))
        
        # year_count <- debate_titles_w_keyword %>%
        #    group_by(year) %>%
        #    summarise(debates_per_year = sum(year_count)) %>% 
        #    mutate(keywords = keyword)
        
        year_count <- debate_titles_w_keyword %>%
          group_by(year) %>%
          summarise(debates_per_year = sum(n)) %>%
          mutate(keywords = keyword)
        
        # observeEvent(dim(year_count[2] == 0), {
        #   showModal(modalDialog(paste0("Keyword ", "\"", keyword, "\"", " not found."))) })
        
        all_year_counts <- bind_rows(all_year_counts, year_count) } }
    
    all_year_counts <- all_year_counts %>%
      left_join(words_per_year, by = "year") %>%
      mutate(proportion = debates_per_year/words_per_year)
    
    return(all_year_counts) } else {
      return(d)
    }
  
  
}