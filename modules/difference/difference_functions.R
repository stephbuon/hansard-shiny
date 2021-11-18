input_loop <- function(input, decades, range_start, make_m, add_decade_col, ...) {
  
  out <- data.frame()
  
  for(fdecade in decades) {
    table_dir <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
    word_vectors <- as.matrix(read.table(table_dir, as.is = TRUE))
    
    if(input %in% rownames(word_vectors)) {
      kw = word_vectors[input, , drop = F]
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[range_start:nrow(cos_sim_rom)]) 
      
      colnames(forplot)[1] <- paste0("similarity")
      
      forplot$word <- rownames(forplot)
      
      if (add_decade_col == TRUE) {
        forplot <- forplot %>%
          mutate(decade = fdecade) %>%
          filter(word == ...) } 
      
      if (make_m == TRUE) {
        rownames(forplot) <- NULL
        forplot <- forplot %>%
          select(word) }
      
      out <- bind_rows(out, forplot) } }
  
  if (make_m == TRUE) {
    out <- dplyr::distinct(out) }
  
  return(out) }


get_button <- function(wv_textbox) {
  decades <- c(1800, 1850)
  
  out <- input_loop(wv_textbox, decades, range_start, make_m = TRUE, add_decade_col = FALSE)
  
  b <- out %>%
    drop_na()
  
  if ( nrow(b) >= 8 && ncol(out) == 3 ) { 
    out$all_sim <- abs(out$similarity1 - out$similarity2)
    
    out <- out %>%
      drop_na()
    
    out <- out %>%
      arrange(desc(all_sim))
    
    return(out) } 
  
  else {
    out <- out %>%
      slice(8:16)
    #slice(10:18)
    
    return(out) } }
