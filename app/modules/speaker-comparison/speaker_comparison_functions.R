binary_search <- function(data_for_visualization, input_decade, measurement, speaker1, speaker2) {
  # return debate text for just the speaker values from the top and bottom radio buttons/text box 
  
  data_for_visualization <- data_for_visualization[.(as.numeric(input_decade))]
  
  setkey(data_for_visualization, clean_new_speaker)
  f <- data_for_visualization[.(as.character(speaker1))]
  m <- data_for_visualization[.(as.character(speaker2))]
  data_for_visualization <- bind_rows(f, m)
  
  return(data_for_visualization) }


find_tf_idf <- function(data_for_visualization) {
  # return tf-idf values for the two selected speakers 

  data_for_visualization <- data_for_visualization %>%
    ungroup()

  data_for_visualization <- data_for_visualization %>%
    bind_tf_idf(ngrams, clean_new_speaker, n)

  data_for_visualization <- data_for_visualization %>%
    select(-n) %>%
    rename(n = tf_idf)

  return(data_for_visualization) }


calculate_results <- function(data_for_visualization, decade, measurement, sc_radio_buttons_top, sc_radio_buttons_bottom) {
  
  data_for_visualization <- binary_search(data_for_visualization, decade, measurement, sc_radio_buttons_top, sc_radio_buttons_bottom)
  
  if(measurement == "tf-idf") {
    data_for_visualization <- find_tf_idf(data_for_visualization) }
  
  return(data_for_visualization) }