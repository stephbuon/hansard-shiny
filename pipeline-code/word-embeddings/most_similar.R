library(tidyverse)
library(text2vec)
library(plotly)

# loop followed by non loop

out <- data.frame()

decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910)

for(d in 1:length(decades)) {
  
  fdecade <- decades[d] 
  
  table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
  
  word_vectors <- as.matrix(read.table(table, as.is = TRUE))
  
  
  kw = word_vectors["slave", , drop = F]
  
  cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
  
  forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:21])
  
  colnames(forplot)[1] <- "similarity"
  
  forplot$word <- rownames(forplot)
  
  forplot <- forplot %>%
    mutate(decade = fdecade)
  
  out <- bind_rows(out, forplot)
  
  
}


fig <- plot_ly(data = out, 
               x = ~decade, 
               y = ~similarity,
               mode = "markers+text",
               text = ~word,
               type = "scatter",
               marker = list(color = 'rgb(158,202,225)',
                             size = 15,
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)),
               textposition = "center right",
               height=1000
) %>%
  config(displayModeBar = F)

fig






word_vectors <- as.matrix(read.table("~/projects/hansard-shiny/hansard_word_vectors_1803_1807.txt", as.is = TRUE))
#word_vectors <- as.matrix(read_delim("~/projects/hansard-shiny/hansard_word_vectors_1803_1807.txt", delim = ",", col_names = T))

#word_vectors <- as.matrix(read_csv("~/projects/hansard-shiny/hansard_word_vectors_1803_1807.txt"))

#rownames(word_vectors) <- word_vectors$word

#word_vectors <- word_vectors[,-word]

kw = word_vectors["slave", , drop = F]

cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")

#head(sort(cos_sim_rom[,1], decreasing = T), 40)

#forplot <-as.data.frame(head(sort(cos_sim_rom[,1], decreasing = T), 20))

forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:21])
  
colnames(forplot)[1] <- "similarity"

forplot$word <- rownames(forplot)

forplot <- forplot %>%
  mutate(decade = 1800)


fig <- plot_ly(data = forplot, 
               x = ~decade, 
               y = ~similarity,
               mode = "markers+text",
               text = ~word,
               type = "scatter",
               marker = list(color = 'rgb(158,202,225)',
                             size = 15,
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)),
               textposition = "center right",
               width=500,
               height=1000
               ) %>%
  config(displayModeBar = F)


# text = rownames(dat), 



fig
