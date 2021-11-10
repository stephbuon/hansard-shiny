library(tidyverse)
library(plotly)

hansard <- read_csv("~/hansard_justnine_w_year.csv")

decade <- 10

hansard <- hansard %>%
  mutate(decade = year - year %% decade)

hansard <- hansard %>%
  select(debate_id, decade)

a <- hansard %>%
  distinct(decade, debate_id) %>%
  group_by(decade) %>%
  summarize(no_of_debates = n())

write_csv(a, "~/number_of_debates_from_1803_1910.csv")

fig <- plot_ly(data=a, 
               x = ~decade, 
               y = ~no_of_debates, 
               type = 'bar', 
               text = ~no_of_debates,
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5))) %>% 
                 layout(title = "Number of Debates From 1803—1910",
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))


fig
