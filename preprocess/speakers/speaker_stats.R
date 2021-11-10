library(tidyverse)

hansard <- read_csv("~/hansard_justnine_w_year.csv") %>%
  select(speech_id, year, text)

period <- 10

hansard <- hansard %>%
  mutate(decade = year - year %% period)

hansard <- hansard %>%
  select(speech_id, decade, text)

split <- str_split(hansard$text, " ")

hansard$row_count <- sapply(split, length)

# the average can be misleading for datasets that are skewed 
# average is useful for normal distribution type problems 
# (for example, average us income you want median since high income people can skew the no to be high -- 100,000 a year)

speech_stats <- hansard %>%
  group_by(decade)%>% 
  summarise(mean=mean(row_count), max=max(row_count), min=min(row_count), median=median(row_count))

write_csv(speech_stats, "~/hansard_c19_speech_statistics.csv")

#fig <- plot_ly(test, 
#               x = ~decade, 
#               y = ~median, 
#               type = 'scatter', 
#               mode = 'lines',
#               line = list(color = 'rgb(158,202,225)',
#                           width = 4))
#fig
