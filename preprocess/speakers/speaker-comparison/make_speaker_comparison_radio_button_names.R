h <- fread("~/projects/hansard-shiny/app-data/speakers/speaker_comparison_speaker_count_for_app_2.csv") 

h <- h %>%
  distinct(decade, new_speaker, clean_new_speaker)


write_csv(h, "~/projects/hansard-shiny/app-data/speakers/speaker_comparison_radio_button_names.csv")


h[1,5]

h <- h %>%
  filter(decade == 1800) %>%
  distinct(decade, new_speaker, clean_new_speaker)