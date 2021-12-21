library(tidyverse)

code_dir <- "/home/stephbuon/projects/hansard-shiny/preprocess/hansard-corpus/hansard-corpus-functions/"
source(paste0(code_dir, "add_decade_col.R"))
source(paste0(code_dir, "remove_symbols.R"))
source(paste0(code_dir, "standardize_speaker_cols.R"))


#
# remove <- c("\\[", "\\]", "\\(", "\\)", "—", "-", "\\.$", "\\*", "\"", "\\,", "\'", "\\>", "^ ", "\\.", " $", "  $")
# remove <- paste0(remove, collapse = '|' )
#
# df$debate <- str_replace_all(df$debate, remove, "")


data_dir <- ""
hansard_speakers <- read_csv("/scratch/group/pract-txt-mine/hansard_c19_improved_speaker_names_2.csv")
hansard <- read_csv("~/hansard_justnine_w_year.csv")

hansard <- add_decade_col(hansard, hansard_speakers)
hansard <- remove_symbols(hansard)
hansard <- standardize_speaker_cols(hansard)

write_csv(hansard, "/scratch/group/pract-txt-mine/sbuongiorno/hansard_c19_improved_speaker_names_app_data.csv")
