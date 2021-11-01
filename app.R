# download multiple plots: https://stackoverflow.com/questions/32320135/how-to-download-several-png-plots-within-one-download-button-in-shiny

# If I want to see the full network, I can pass VisNetwork the static edges data (e1) instead of the reactive expression (reactive_edges()). 

# keep from recalculating: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

# Q for when I run static data: why does graph get smaller with more nodes? I think bc I am then selecting nodes with, say, London AND Lymric. Not 100% sure.

# https://docs.google.com/document/d/1DN5xy1WlA_nqW0xUtG0ErJbhqSfuURB3G5HSr9Swnxc/edit

# connections is the number of word pairs in which the node appears
# total weight -- IDK

# I don't keep ambig like saint johns nation or church

# add favorite words for speaker -- click on point and return DF with that info 


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(visNetwork)
library(plotly)
library(viridis)
library(tidyverse)
library(tidytext)
library(data.table)
library(DT)
library(stringi)
library(reshape2)
library(text2vec)
library(ggwordcloud)
library(scales)

#library(bslib) # for more themes
#library(ggrepel)

modules_dir <- "~/projects/hansard-shiny/modules/"

source(paste0(modules_dir, "introduction/introduction.R"))
source(paste0(modules_dir, "nation-concerns/nation_concerns.R"))
source(paste0(modules_dir, "nation-pairs/nation_pairs.R"))
source(paste0(modules_dir, "longest-speeches/longest_speeches.R"))
source(paste0(modules_dir, "similarity/similarity.R"))
source(paste0(modules_dir, "purpose/purpose.R"))
source(paste0(modules_dir, "code/code.R"))
source(paste0(modules_dir, "data/data.R"))
source(paste0(modules_dir, "network/network.R"))
source(paste0(modules_dir, "debate-titles/debate_titles.R"))
source(paste0(modules_dir, "top-speakers/top_speakers.R"))
source(paste0(modules_dir, "longest-debates/longest_debates.R"))
source(paste0(modules_dir, "collocates/collocates.R"))
source(paste0(modules_dir, "speech-lengths/speech_lengths.R"))
source(paste0(modules_dir, "difference/difference.R"))
source(paste0(modules_dir, "speaker-comparison/speaker_comparison.R"))

ui <- fluidPage(
  
  tags$head(tags$style(HTML(".shiny-output-error-validation { 
                            color: #696969;
                            text-align: center;
                            font-size: 18px; } "))),
  
  theme = shinytheme("yeti"),
  #theme = bs_theme(bootswatch = "litera"),
  
  navbarPage("The Hansard Parliamentary Debates",

             tabPanel("Introduction",
                      introduction_ui("ndbs")),
 
             tabPanel("Triples Network", 
                      network_ui("network")),
             
             navbarMenu("Nations",
                        tabPanel("Nation Concerns",
                                 nation_concerns_ui("nation_concerns")),
         
                        tabPanel("Nation Pairs",
                                 nation_pairs_ui("nation_pairs"))),
             
             tabPanel("Collocates",
                      collocates_ui("collocates")),
             
             navbarMenu("Speakers",
                        tabPanel("Top Speakers",
                                 top_speakers_ui("top_speakers")),
                        
                        tabPanel("Longest Speeches",
                                 longest_speeches_ui("longest_speeches")),
                        
                        tabPanel("Speech Lengths",
                                 speech_lengths_ui("speech_lengths")),
                        
                        tabPanel("Speaker Comparison",
                                 speaker_comparison_ui("speaker-comparison"))),

             navbarMenu("Debates",
                        tabPanel("Debate Titles",
                                 debate_titles_ui("debate_titles")),
                        
                        tabPanel("Longest Debates",
                                 longest_debates_ui("longest_debates"))),
             
             navbarMenu("Context",
                        tabPanel("Try 1",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Say something meaningful"),
                                     width = 2),
                                   mainPanel(plotlyOutput("word_embeddings")))),
                        
                        tabPanel("Similarity",
                                 similarity_ui("similarity")),
                        
                        tabPanel("Difference",
                                 difference_ui("wv_test"))),

             navbarMenu("About",
                        tabPanel("Purpose",
                                 purpose_ui()),
                        tabPanel("Code", 
                                 code_ui()),
                        tabPanel("Data",
                                 data_ui())) 
             
             ) ) 



server <- function(input, output, session) {

  introduction_server("ndbs")
  network_server("network")
  nation_concerns_server("nation_concerns")
  nation_pairs_server("nation_pairs")
  similarity_server("similarity")
  debate_titles_server("debate_titles")
  longest_speeches_server("longest_speeches")
  longest_debates_server("longest_debates")
  top_speakers_server("top_speakers")
  collocates_server("collocates")
  speech_lengths_server("speech_lengths")
  difference_server("wv_test")
  speaker_comparison_server("speaker-comparison")
  
  
  output$speech_stats <- renderPlotly({
    speech_stats <- fread("~/projects/hansard-shiny/app-data/speakers/speech_stats.csv")
    
    plot_ly(speech_stats, 
            x = ~decade, 
            y = ~median, 
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(158,202,225)',
                        width = 4))  })
  
  
  
  
  forplot11 <- fread("~/projects/hansard-shiny/app-data/word_embeddings/for_plot.csv")
  output$word_embeddings <- renderPlotly({
    
    plot_ly(data = forplot11, 
            x = ~V1, 
            y = ~V2, 
            type = 'scatter', 
            mode = 'text',
            text = ~word) %>% 
      layout(#autosize = F,
        xaxis = list(title = "Placeholder"),
        yaxis = list(title = "Placeholder")) %>%
      config(displayModeBar = F) })
  
  
  
}

shinyApp(ui, server)

