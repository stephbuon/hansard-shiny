library(tidyverse)
library(reshape2)

nations_count <- read_csv("~/projects/hansard-shiny/hansard_c19_debate_title_nation_count.csv")

nations_count$decade <- as.character(nations_count$decade)



spain <- nations_count %>%
  filter(debate == "Spain") %>%
  rename(scount = n)

spain <- dcast(spain, decade ~ debate)


portugal <- nations_count %>%
  filter(debate == "Portugal") %>%
  rename(pcount = n)

portugal <- dcast(portugal, decade ~ debate)

all <- left_join(spain, portugal, on = "decade")


fig <- plot_ly(all, 
               x = ~decade, 
               y = ~Spain, 
               type = 'bar',
               marker= list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5)), 
               name = 'SF Zoo')
fig <- fig %>% 
  add_trace(y = ~Portugal, 
            marker= list(color = 'rgb(58,200,225)',
                         line = list(color = 'rgb(8,48,107)',
                                     width = 1.5)),
            name = 'LA Zoo') %>% 
  layout(yaxis = list(title = 'Count'), barmode = 'group') %>%
  config(displayModeBar = F)

fig
