export_speaker_comparison_radio_buttons <- function(hansard) {
  
  hansard <- hansard %>%
    distinct(decade, new_speaker, clean_new_speaker)
  
  fwrite(hansard, paste0(app_data_dir, "speaker_comparison_radio_button_names.csv"))
  
  #hansard[1,5]
  
  #hansard <- hansard %>%
  #  filter(decade == 1800) %>%
  #  distinct(decade, new_speaker, clean_new_speaker)
  
  
  
  
  
}

