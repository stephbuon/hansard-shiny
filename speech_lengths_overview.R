# sort short, mid-range, long
library(tidyverse)
library(scales)

hansard <- read_csv("~/projects/hansard-shiny/speech_lengths.csv") %>%
  rename(speech_length = n)

short_speech <- hansard %>%
  filter(speech_length > 0,
         speech_length < 50) %>%
  mutate(speech_length_type = 0,
         `Speech Length` = "short")

mid_range_speech <- hansard %>%
  filter(speech_length > 49,
         speech_length < 1000) %>%
  mutate(speech_length_type = 1,
         `Speech Length`  = "mid")

long_speech <- hansard %>%
  filter(speech_length > 999) %>%
  mutate(speech_length_type = 2,
         `Speech Length`  = "long")

all_labeled_speeches <- bind_rows(short_speech, mid_range_speech, long_speech)

viz <- all_labeled_speeches %>%
  count(decade, `Speech Length`, speech_length_type)

write_csv(viz, "~/projects/hansard-shiny/speech_lengths_overview.csv")

labs <- c("Short", "Mid-Range", "Long")

plot <- ggplot(viz, 
       aes(x = speech_length_type,
           y = n,
           fill = `Speech Length`,
           show.legend = FALSE)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(labels = labs) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~decade) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_bw() 

ggplotly(plot) %>%
  config(displayModeBar = F)
