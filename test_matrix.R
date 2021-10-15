


decades <- c(1810, 1820, 1830, 1840, 1850, 1860)

table_1 <- paste0("~/projects/hansard-shiny/hansard_word_vectors_1800.txt")

out <- as.matrix(read.table(table_1, as.is = TRUE)) 

for(d in 1:length(decades)) {
  
  fdecade <- decades[d] 
  
table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
word_vectors <- as.matrix(read.table(table, as.is = TRUE))


out <- rbind(out, word_vectors)

}

#out<-out[out[ ,1]!=1, ]

out_2 <- as.data.frame(out)

out_2$word <- row.names(out_2) 


out_2 <- out_2 %>% 
  filter(!grepl("[[:digit:]]", word)) %>%
  select(-word)

out_2 <- as.matrix(out_2)


write.table(out_2, file="~/projects/hansard-shiny/test_matrix.txt")
