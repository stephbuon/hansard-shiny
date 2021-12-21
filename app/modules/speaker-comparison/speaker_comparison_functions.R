binary_search <- function(j, input_decade, input_sc_compare, speaker1, speaker2) {
  j <- j[.(as.numeric(input_decade))]
  
  setkey(j, clean_new_speaker)
  f <- j[.(as.character(speaker1))]
  m <- j[.(as.character(speaker2))]
  j <- bind_rows(f, m)
  
  if (input_sc_compare == "sc_tf-idf") {
    j <- tf_idf_b(j, speaker1, speaker2) } # split after the function 
  
  return(j) }


tf_idf_b <- function(df, dct, dcb) {
      
      df <- df %>%
        filter(clean_new_speaker == dct | clean_new_speaker == dcb) 
      
      df <- df %>%
        ungroup()
      
      df <- df %>%
        bind_tf_idf(ngrams, clean_new_speaker, n)
      
      
      df <- df %>%
        select(-n) %>%
        rename(n = tf_idf) 
      
      return(df) }


