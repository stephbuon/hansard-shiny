# download multiple plots: https://stackoverflow.com/questions/32320135/how-to-download-several-png-plots-within-one-download-button-in-shiny

# If I want to see the full network, I can pass VisNetwork the static edges data (e1) instead of the reactive expression (reactive_edges()).

# Q for when I run static data: why does graph get smaller with more nodes? I think bc I am then selecting nodes with, say, London AND Lymric. Not 100% sure.

# connections is the number of word pairs in which the node appears
# total weight -- IDK

# I don't keep ambig like saint johns nation or church

server <- FALSE

if (server == TRUE) {
  data_dir <- "./app-data/"
  modules_dir <- "./modules/" 
  } else {
  data_dir <- "~/projects/hansard-shiny/app/app-data/"
  modules_dir <- "~/projects/hansard-shiny/app/modules/" }



library(shiny)
library(shinythemes)
library(shinyWidgets)
library(visNetwork)
library(plotly)
library(viridis)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(stringi)
library(tidytext)
library(data.table)
library(DT)
library(reshape2)
library(text2vec)
library(ggwordcloud)
library(scales)
library(memoise)
library(quanteda)


# load functions
source(paste0(modules_dir, "global_functions.R"))
source(paste0(modules_dir, "collocates/collocates_functions.R"))
source(paste0(modules_dir, "debate-titles/debate_titles_functions.R"))
source(paste0(modules_dir, "difference/difference_functions.R"))
source(paste0(modules_dir, "kwic/kwic_functions.R"))
source(paste0(modules_dir, "speaker-comparison/speaker_comparison_functions.R"))
source(paste0(modules_dir, "speech-lengths/speech_length_annotations.txt"))

# enable caching 
source(paste0(modules_dir, "kwic/kwic_cache.R"))

# load apps
source(paste0(modules_dir, "introduction/introduction.R"))
source(paste0(modules_dir, "nation-concerns/nation_concerns.R"))
source(paste0(modules_dir, "nation-pairs/nation_pairs.R"))
source(paste0(modules_dir, "longest-speeches/longest_speeches.R"))
#source(paste0(modules_dir, "word-context/word_context.R"))
source(paste0(modules_dir, "purpose/purpose.R"))
source(paste0(modules_dir, "code/code.R"))
source(paste0(modules_dir, "data/data.R"))
source(paste0(modules_dir, "network/network.R"))
source(paste0(modules_dir, "debate-titles/debate_titles.R"))
source(paste0(modules_dir, "top-speakers/top_speakers.R"))
source(paste0(modules_dir, "longest-debates/longest_debates.R"))
#source(paste0(modules_dir, "collocates/collocates.R"))
source(paste0(modules_dir, "speech-lengths/speech_lengths.R"))
source(paste0(modules_dir, "difference/difference.R"))
source(paste0(modules_dir, "speaker-comparison/speaker_comparison.R"))
source(paste0(modules_dir, "vector-space/vector_space.R"))
source(paste0(modules_dir, "context/context.R"))


ui <- fluidPage(

  tags$head(tags$style(HTML(".shiny-output-error-validation {
                            color: #696969;
                            text-align: center;
                            font-size: 18px; } "))),

  theme = shinytheme("yeti"),

  navbarPage("The Hansard Parliamentary Debates",

             tabPanel("Introduction",
                      introduction_ui("ndbs")),

             # tabPanel("Time",
             #          collocates_ui("collocates")),

             navbarMenu("Speakers",

                        tabPanel("Longest Speeches",
                                 longest_speeches_ui("longest_speeches")),

                        tabPanel("Top Speakers",
                                 top_speakers_ui("top_speakers")),

                        tabPanel("Triples Network",
                                 network_ui("network")),

                        tabPanel("Speaker Comparison",
                                 speaker_comparison_ui("speaker-comparison"))),

             navbarMenu("Debate Titles",
                        tabPanel("Debate Titles",
                                 debate_titles_ui("debate_titles")),

                        tabPanel("Nation Concerns",
                                 nation_concerns_ui("nation_concerns")),

                        tabPanel("Nation Pairs",
                                 nation_pairs_ui("nation_pairs"))),

             navbarMenu("Debate Text",
                        tabPanel("Longest Debates",
                                 longest_debates_ui("longest_debates")),

                        tabPanel("Speech Lengths",
                                 speech_lengths_ui("speech_lengths"))),

             navbarMenu("Language",
                        tabPanel("Vector Space",
                                vector_space_ui("vector_space")),

                        tabPanel("Word Context",
                                 context_ui("context")),
                                 #word_context_ui("word_context")),

                        tabPanel("Difference",
                                 difference_ui("difference"))),

             navbarMenu("About",
                        tabPanel("Purpose",
                                 purpose_ui()),
                        tabPanel("Code",
                                 code_ui()),
                        tabPanel("Data",
                                 data_ui()),
                        tabPanel("Report a Bug",
                                 ))

             ) )



server <- function(input, output, session) {

  introduction_server("ndbs")
  network_server("network")
  nation_concerns_server("nation_concerns")
  nation_pairs_server("nation_pairs")
  #word_context_server("word_context")
  debate_titles_server("debate_titles")
  longest_speeches_server("longest_speeches")
  longest_debates_server("longest_debates")
  top_speakers_server("top_speakers")
  #collocates_server("collocates")
  speech_lengths_server("speech_lengths")
  difference_server("difference")
  speaker_comparison_server("speaker-comparison")
  vector_space_server("vector_space")
  context_server("context") # == collocates + word_context

  }

shinyApp(ui, server)

