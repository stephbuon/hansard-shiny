h <- read_csv("~/projects/hansard-shiny/speaker_comparison_speaker_count_for_app.csv")

h <- h %>%
  mutate(clean_new_speaker = new_speaker)

h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "_", " ") 
h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "[:digit:]", "")

h$clean_new_speaker <- str_to_title(h$clean_new_speaker)

write_csv(h, "~/projects/hansard-shiny/speaker_comparison_speaker_count_for_app_2.csv")

t <- h %>%
  distinct(decade, new_speaker)


