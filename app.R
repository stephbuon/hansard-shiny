# remember that rN the network data is in network/cleaning.R

# download multiple plots: https://stackoverflow.com/questions/32320135/how-to-download-several-png-plots-within-one-download-button-in-shiny

# remember to add cache stuff 
# If I want to see the full network, I can pass VisNetwork the static edges data (e1) instead of the reactive expression (reactive_edges()). 

# keep from recalculating: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

# Q for when I run static data: why does graph get smaller with more nodes? I think bc I am then selecting nodes with, say, London AND Lymric. Not 100% sure.

# https://docs.google.com/document/d/1DN5xy1WlA_nqW0xUtG0ErJbhqSfuURB3G5HSr9Swnxc/edit

# connections is the number of word pairs in which the node appears
# total weight -- IDK

# google drive: https://docs.google.com/document/d/15rDOVGFlx8JF7DmysuqGS02Be7air8wka_J6dxL4k9I/edit

# I don't keep ambig like saint johns nation or church

# add favorite words for speaker -- click on point and return DF with that info 

#library(bslib) # for more themes
#library(ggrepel)

# triples network could instead be nations 

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


vals = reactiveValues(btn = FALSE, text = FALSE)
top_vals_speaker_comparison = reactiveValues(btn = FALSE, text = FALSE)
bottom_vals_speaker_comparison = reactiveValues(btn = FALSE, text = FALSE)



########### UI 

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
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("about_speech_lengths", 
                                                  "About This Page",
                                                  style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
                                     p(),
                                     selectInput("drop_down_hist", 
                                                 "Speeches:",
                                                 c("Overview" = "overview",
                                                   "Short Speeches (1-49 words)" = "Short (1-49 Word)",
                                                   "Mid-Length Speeches (50-999 words) " = "Mid-Length (50-999 Word)",
                                                   "Long Speeches (1000+ words)" = "long")),
                                     
                                     
                                     conditionalPanel(
                                       condition = "input.drop_down_hist != 'overview'",
                                       sliderTextInput(
                                         inputId = "decade_hist", 
                                         label = "Decade: ", 
                                         grid = TRUE, 
                                         force_edges = TRUE,
                                         choices = c("1800",
                                                     "1810", 
                                                     "1820", 
                                                     "1830", 
                                                     "1840",
                                                     "1850", 
                                                     "1860",
                                                     "1870",
                                                     "1880",
                                                     "1890")),
                                       
                                       sliderInput(inputId = "bins",
                                                   label = "Number of bins:",
                                                   min = 5,
                                                   max = 25,
                                                   value = 15, 
                                                   step = 10) ),
                                     
                                     width = 2),
                                   
                                   mainPanel(
                                     plotlyOutput("value_plot"),
                                     DTOutput('speech_lengths_table')))),
                        
                        tabPanel("Speaker Comparison",
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("about_nation_pairs", 
                                                  "About This Page",
                                                  style="color: #fff;
                                                  background-color: #337ab7; 
                                                  border-color: #2e6da4; 
                                                  width: 179px;
                                                  padding:4px; 
                                                  font-size:90%"),
                                     p(),
                                     selectInput("sc_compare", "Compare:",
                                                 c("Top Words" = "sc_top_words",
                                                   "tf-idf" = "sc_tf-idf",
                                                   "Speech Lengths" = "sc_speech_lengths")),
                                     
                                     sliderTextInput(
                                       inputId = "sc_decade", 
                                       label = "Decade: ", 
                                       grid = TRUE, 
                                       force_edges = TRUE,
                                       choices = c("1800",
                                                   "1810", 
                                                   "1820", 
                                                   "1830", 
                                                   "1840",
                                                   "1850", 
                                                   "1860",
                                                   "1870",
                                                   "1880",
                                                   "1890")),
                                     
                                     radioButtons("sc_radio_buttons_top", 
                                                  "Top Speakers:",
                                                  #choices = NULL),
                                                  
                                                   c("1" = "1",
                                                     "2" = "2",
                                                     "3" = "3",
                                                     "4" = "4",
                                                     "5" = "5"),
                                                   selected = "1"),
                                     textInput("sc_custom_search_top", "Custom Search:", ""),
                                     
                                     radioButtons("sc_radio_buttons_bottom", 
                                                  "Top Speakers:",
                                                  #choices = NULL),
                                                   c("1" = "1",
                                                     "2" = "2",
                                                     "3" = "3",
                                                     "4" = "4",
                                                     "5" = "5"),
                                                   selected = "1"),
                                     textInput("sc_custom_search_bottom", "Custom Search:", ""),
                                     actionButton('download_speaker_comparison', 
                                                  "Download Plot",
                                                  style = "width: 179px;"
                                     ),
                                     
                                     width = 2),
                                 mainPanel(plotlyOutput("speaker_comparison_top"),
                                           plotlyOutput("speaker_comparison_bottom"))))),

             
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
                         sidebarLayout(
                           sidebarPanel(
                             actionButton("about_difference", 
                                          "About This Page",
                                          style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
                             p(),
                             textInput("wv_textbox", "Keyword:", "tenant"),
                             uiOutput("wv_action_button",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_2",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_3",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_4",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_5",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_6",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_7",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             uiOutput("wv_action_button_8",
                                      onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
                             br(),
                             tags$style("#wv_text_box_1 {background-color:#E8E8E8;}"),
                             textInput("wv_text_box_1", 
                                       "Custom Search:", ""),
                             br(),
                             actionButton('download_try_2', 
                                          "Download Plot",
                                          style = "width: 179px;"
                             ),
                             width = 2),
                           
                           mainPanel(plotlyOutput("wv_test", height = "650"),
                                     
                                     tags$script('document.getElementById("download_try_2").onclick = function() {
                                     var gd = document.getElementById("wv_test");
                                     Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                                     var a = window.document.createElement("a");
                                     a.href = url; 
                                     a.type = "image/png";
                                     a.download = "plot.png";
                                     document.body.appendChild(a);
                                     a.click();
                                     document.body.removeChild(a);
                                     });
                                     } ')
                                     
                                     
                                     )
                           ) ) ),
      
             
             navbarMenu("About",
                        tabPanel("Purpose",
                                 purpose_ui()),
                        tabPanel("Code", 
                                 code_ui()),
                        tabPanel("Data",
                                 data_ui() 
                                 
                                 ) ) ) ) 



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
  
  
  
  
  
  output$speech_lengths <- renderPlotly({
    speech_stats <- fread("~/projects/hansard-shiny/app-data/speakers/speech_stats.csv")
    
    plot_ly(speech_stats, 
            x = ~decade, 
            y = ~median, 
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(158,202,225)',
                        width = 4))  })
  
  
  
  
  viz <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths_overview.csv")
  hansard_speech_lengths <- fread("~/projects/hansard-shiny/app-data/speakers/speech_lengths.csv") #%>%
    #rename(speech_length = n)
  
  output$value_plot <- renderPlotly({
    
    if (input$drop_down_hist == "overview") {
      
      labs <- c("Short", "Mid-Length", "Long")
      
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
        layout(height = 650) %>%
        config(displayModeBar = F)
      
      
      
      
    } else {
      
      
      hansard_speech_lengths <- hansard_speech_lengths %>%
        filter(decade == input$decade_hist) %>%
        select(-decade)

      hansard_speech_lengths <- hansard_speech_lengths %>%
         filter(case_when(input$drop_down_hist == "Short (1-49 Word)" ~ speech_length > 0 & speech_length < 50, 
                          input$drop_down_hist == "Mid-Length (50-999 Word)" ~ speech_length > 49 & speech_length < 1000,
                          input$drop_down_hist == "long" ~ speech_length > 999))
       

      d <- hansard_speech_lengths %>%
        add_count(speech_length) 
      
      render_value_range_hist(d) 
      
      plot_ly(x = d$speech_length, 
              nbinsx = input$bins,
              type = "histogram",
              source="YYY",
              marker = list(color = 'rgb(158,202,225)')) %>%
        layout(title = paste0(input$drop_down_hist, " Speeches In ", input$decade_hist),
               bargap = 0.1) %>%
        config(displayModeBar = F) }
    
  })
  
  
  
  
  render_value_range_hist <- function(d){
    
    output$speech_lengths_table <- renderDT({
      
      if (input$drop_down_hist == "overview") {
      } else {
        
        s <- event_data("plotly_click", source = "YYY")
        
        validate(need(!is.null(s), "Click on a bar to view binned speeches"))
        
        
        if (input$drop_down_hist == "Mid-Length (50-999 Word)") {
          
          if (input$bins == 15) {
            top <- s$x + 50.5
            bottom <- s$x - 49.5 }
          else if (input$bins == 5) {
            top <- s$x + 99.5
            bottom <- s$x - 99.5 } 
          else if (input$bins == 25) {
            top <- s$x + 24.5
            bottom <- s$x - 24.5 } } 
        
        else if (input$drop_down_hist == "Short (1-49 Word)") {
          if (input$bins == 15) {
            top <- s$x + 3
            bottom <- s$x - 3 } 
          else if (input$bins == 25) { 
            top <- s$x + 1.5
            bottom <- s$x - 1.5 }
          else if (input$bins == 5) {
            top <- s$x + 5.5
            bottom <- s$x - 5.5 } }
        
        else if (input$drop_down_hist == "long") {
          if (input$bins == 15) {
            print(s) }
          
        }
        
        d <- d %>%
          filter(speech_length < top,
                 speech_length > bottom)
        
        datatable(d,
                  options = list(dom = 'ip'),
                  filter = list(position = "top"))
      }
      
    })  
  }           
  
  
  
  
  ######### speaker comparison
  h <- fread("~/projects/hansard-shiny/app-data/speakers/speaker_comparison_speaker_count_for_app_2.csv")
  j <- fread("~/projects/hansard-shiny/clean_clean_tokenized_hansard_counts.csv", key = "decade")
  
  observeEvent(input$sc_radio_buttons_top,{
    top_vals_speaker_comparison$btn = TRUE
    top_vals_speaker_comparison$text = FALSE })
  
  observeEvent(input$sc_custom_search_top,{
    top_vals_speaker_comparison$btn = FALSE
    top_vals_speaker_comparison$text = TRUE })

  observeEvent(input$sc_radio_buttons_bottom,{
    bottom_vals_speaker_comparison$btn = TRUE
    bottom_vals_speaker_comparison$text = FALSE })
  
  observeEvent(input$sc_custom_search_bottom,{
    bottom_vals_speaker_comparison$btn = FALSE
    bottom_vals_speaker_comparison$text = TRUE })
  
  
  
  
  observe({
    h <- h %>%
      filter(decade == input$sc_decade) %>%
      distinct(decade, new_speaker, clean_new_speaker)
      
      updateRadioButtons(session, "sc_radio_buttons_top",
                         choices = h$clean_new_speaker)
      
      updateRadioButtons(session, "sc_radio_buttons_bottom",
                         choices = h$clean_new_speaker) })
  
  
  
  
  tf_idf_b <- function(df, dct, dcb) {
    
    df <- df %>%
      filter(clean_new_speaker == dct | clean_new_speaker == dcb) 
    
    df <- df %>%
      ungroup()
    
    df <- df %>%
      bind_tf_idf(ngrams, clean_new_speaker, n)

    
    df <- df %>%
      select(-n) %>%
      rename(n = tf_idf) 
    
    return(df) }
  
  
  binary_search <- function(j, speaker1, speaker2) {
    j <- j[.(as.numeric(input$sc_decade))]

    setkey(j, clean_new_speaker)
    f <- j[.(as.character(speaker1))]
    m <- j[.(as.character(speaker2))]
    j <- bind_rows(f, m)
    
    if (input$sc_compare == "sc_tf-idf") {
      j <- tf_idf_b(j, speaker1, speaker2) } # split after the function 
    
    return(j) }
  
  
  test_2 <- function (j, top_or_bottom) {
    if (top_vals_speaker_comparison$btn & bottom_vals_speaker_comparison$btn) {
      j <- binary_search(j, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) }
    
    else if (top_vals_speaker_comparison$text & bottom_vals_speaker_comparison$btn) {
      j <- binary_search(j, input$sc_custom_search_top, input$sc_radio_buttons_bottom) }
    
    else if (top_vals_speaker_comparison$btn & bottom_vals_speaker_comparison$text) {
      j <- binary_search(j, input$sc_radio_buttons_top, input$sc_custom_search_bottom) }
    
    else if (top_vals_speaker_comparison$text & bottom_vals_speaker_comparison$text) {
      j <- binary_search(j, input$sc_custom_search_top, input$sc_custom_search_bottom) } 
    
    
    
    
    # if (paste0(top_or_bottom, "_vals_speaker_comparison$btn")) {
    #   speaker <- input$sc_radio_buttons_top
    #   
    #   j <- j %>%
    #     filter(clean_new_speaker == speaker) %>%
    #     distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }
    # 
    # else if (paste0(top_or_bottom, "_vals_speaker_comparison$text")) {
    #   speaker <- input$sc_custom_search_top # maybe I can do paste zero and just take the value of top_or_bottom
    #   
    #   j <- j %>%
    #     filter(clean_new_speaker == speaker) %>%
    #     distinct(new_speaker, decade, ngrams, n, clean_new_speaker) } 
    # 
    # 
    # 
    # return(j)} 
    
    
    
    if (top_or_bottom == "top") {

      if (top_vals_speaker_comparison$btn) {
        speaker <- input$sc_radio_buttons_top

        j <- j %>%
          filter(clean_new_speaker == speaker) %>%
          distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }

      else if (top_vals_speaker_comparison$text) {
        speaker <- input$sc_custom_search_top # maybe I can do paste zero and just take the value of top_or_bottom

        j <- j %>%
          filter(clean_new_speaker == speaker) %>%
          distinct(new_speaker, decade, ngrams, n, clean_new_speaker) } }

    else if (top_or_bottom == "bottom") {
      if (bottom_vals_speaker_comparison$btn) {
        speaker <- input$sc_radio_buttons_bottom

        j <- j %>%
          filter(clean_new_speaker == speaker) %>%
          distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }

      else if (bottom_vals_speaker_comparison$text) {
        speaker <- input$sc_custom_search_bottom

        j <- j %>%
          filter(clean_new_speaker == speaker) %>%
          distinct(new_speaker, decade, ngrams, n, clean_new_speaker) } }
    
    
    
    
    j <- j[order(j, -n)]
    top <- j[1:20]  

    return(top) }

  
  output$speaker_comparison_top <- renderPlotly({
    
    ll <- test_2(j, "top") 
    
    if(top_vals_speaker_comparison$btn){
      ff <- input$sc_radio_buttons_top } 
    else {
      ff <- input$sc_custom_search_top }
       
    
    
    if (input$sc_compare == "sc_top_words") {
      xlab <- list(title ="Frequency") } 
    else if (input$sc_compare == "sc_tf-idf") {
      xlab <- list(title ="tf-idf") }
    
    
    plot_ly(ll, 
            x = ~n, 
            y = ~reorder(ngrams, n), 
            type = 'bar', 
            text = n,
            orientation = "h",
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = ff,
             xaxis = list(title = xlab),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F) 
    
  })
  
  
  

  
  # IF TEXT IS TRUE GET RID OF RADIO BOX 

  
  
  output$speaker_comparison_bottom <- renderPlotly({

    ll <- test_2(j, "bottom") 
    
    
    if(bottom_vals_speaker_comparison$btn){
      ff <- input$sc_radio_buttons_bottom } 
    else {
      ff <- input$sc_custom_search_bottom }

    if (input$sc_compare == "sc_top_words") {
      xlab <- list(title ="Frequency") } 
    else if (input$sc_compare == "sc_tf-idf") {
      xlab <- list(title ="tf-idf") }
    
    
    
    plot_ly(ll, 
            x = ~n, 
            y = ~reorder(ngrams, n), 
            type = 'bar', 
            text = n,
            orientation = "h",
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = ff,
             xaxis = list(title = xlab),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F) 
  })
  
  
  
  
  
  

  
  
  #################### Word Embeddings ###################################################################################
  ########################################################################################################################
  
  #################### Try 1 
  
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
  
  
  
  #################### Try 2 
  
  output$wv_action_button <- renderUI({
    forplot <- get_button() 
    if (nrow(forplot) > 0) {
      actionButton("wv_action_button", label = forplot[1,1], style = "width: 179px;") } else { # 1,2
        return() } 
    })
  
  output$wv_action_button_2 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_2", label = forplot_2[2,1], style = "width: 179px;") } else { # 2,2
        return() } 
    })
  
  output$wv_action_button_3 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_3", label = forplot_2[3,1], style = "width: 179px;") } else { # 3,2
        return() } 
    })
  
  output$wv_action_button_4 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_4", label = forplot_2[4,1], style = "width: 179px;") } else { # 4,2
        return() }
  })
  
  output$wv_action_button_5 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_5", label = forplot_2[5,1], style = "width: 179px;") } else { # 5,2 
        return() } })
  
  output$wv_action_button_6 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_6", label = forplot_2[6,1], style = "width: 179px;") } else { # 6,2 
        return() }
  })
  
  output$wv_action_button_7 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_7", label = forplot_2[7,1], style = "width: 179px;") } else { 
        return() }
  })
  
  
  output$wv_action_button_8 <- renderUI({
    forplot_2 <- get_button()
    if (nrow(forplot_2) > 0) {
      actionButton("wv_action_button_8", label = forplot_2[8,1], style = "width: 179px;") } else { 
        return() }
  })
  

  input_loop <- function(input, decades, first_range, second_range, make_m, make_decade, ...) {
    
    out <- data.frame()
    il_decades <- decades
    
    for(fdecade in il_decades) {
      #fdecade <- il_decades[d] 
      
      table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
      word_vectors <- as.matrix(read.table(table, as.is = TRUE))
      
      if(input %in% rownames(word_vectors)) {
        kw = word_vectors[input, , drop = F]
        cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
        forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[first_range:second_range])
        colnames(forplot)[1] <- paste0("similarity")
        
        forplot$word <- rownames(forplot)
        
        if (make_decade == TRUE) {
          forplot <- forplot %>%
            mutate(decade = fdecade) %>%
            filter(word == ...) } 
        
        if (make_m == TRUE) {
          rownames(forplot) <- NULL
          forplot <- forplot %>%
            select(word) }
        
        out <- bind_rows(out, forplot) } }
    
    if (make_m == TRUE) {
      out <- dplyr::distinct(out)
    }
    
    
    return(out) }
  
  
  
  get_button <- function() {
    decades <- c(1800, 1850)
    
    out <- input_loop(input$wv_textbox, decades, 2, 401, make_m = TRUE, make_decade = FALSE) # from 201 -- 301 may have been better 

    cycle = 0
    
    for(fdecade in decades) {
      cycle = cycle + 1
      #fdecade <- decades[d] 
      
      table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
      # table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_1800.txt")
      
      word_vectors <- as.matrix(read.table(table, as.is = TRUE))
      
      rn <- rownames(word_vectors)
      if(input$wv_textbox %in% rn) {
        kw = word_vectors[input$wv_textbox, , drop = F]
        
        cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
 
        forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:401]) # from 101
        
        colnames(forplot)[1] <- paste0("similarity", cycle)
        
        forplot$word <- rownames(forplot)
        
        # forplot <- forplot %>%
        #    mutate(decade = fdecade)
        
        rownames(forplot) <- NULL
        
        out <- left_join(out, forplot, by = "word") }}
  
    b <- out %>%
      drop_na()
    
    if ( nrow(b) >= 8 && ncol(out) == 3 ) { 
      
      out$all_sim <- abs(out$similarity1 - out$similarity2)
      
      out <- out %>%
        drop_na()
      
      out <- out %>%
        arrange(desc(all_sim))
      
      return(out) } else {
        
        out <- out %>%
          slice(10:18)
        
        return(out) } }
  

  
  observeEvent(input$btnLabel,{
    vals$btn=TRUE
    vals$text=FALSE })
  
  observeEvent(input$wv_text_box_1,{
    vals$btn=FALSE
    vals$text=TRUE })
  
  

  output$wv_test <- renderPlotly({
    
    decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860)

    if(vals$btn){
    out <- input_loop(input$wv_textbox, decades, 2, 401, make_decade = TRUE, input_button_label = input$btnLabel, make_m = FALSE) # same -- from 201 
    } else {
    out <- input_loop(input$wv_textbox, decades, 2, 401, make_m = FALSE, make_decade = TRUE, input_button_label= input$wv_text_box_1) # same -- from 201 
    }
    

    ### to add zeros
    # for(d in 1:length(decades)) {
    # dec <- decades[d] 
    # if (!dec %in% out$decade) {
    #   similarity <- NA
    #   word <- input$wv_textbox
    #   decade <- dec
    #   out_1 <- data.table(similarity, word, decade)
    #   out <- bind_rows(out, out_1)
    #   out <- out %>%
    #     arrange(decade) }}
    
    if(vals$btn){
      ff <- input$btnLabel
    } else {
      ff <- input$wv_text_box_1
    }
    
    if (isTruthy(input$wv_textbox)) {
      plot_ly(data = out, 
              x = ~decade, 
              y = ~similarity,
              mode = "lines+markers",
              type = "scatter",
              line = list(color = "black", width = 1),
              marker = list(color = 'rgb(158,202,225)',
                            size = 15,
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
        layout(xaxis = list(autotick = F, # why does this not work??
                            tickmode = "array", 
                            tickvals = c(1800, 1810, 1820, 1830, 1840, 1850, 1860),
                            dtick = 10,
                            range = c(1800, 1910)),
               #yaxis = list(rangemode = "tozero"),
               title = paste0("Relationship of ", "\"", ff, "\"", " to ", "\"", input$wv_textbox, "\"", " over time")) %>%
        config(displayModeBar = F) } else {
          
          df <- data.frame(decade=integer(),
                           similarity=integer())
          
          plot_ly(data = df,
                  x = ~decade,
                  y = ~similarity) %>%
            config(displayModeBar = F) } })
  

  
  
  
  
  observeEvent(input$about_difference, {
    showModal(modalDialog(
      title = "Debate Titles: Proportion of Debate Titles that Include Keywords",
      "Define",
      br(),
      p(),
      strong("Controls:"),
      "Use the \"Keywords List\" drop down box to select a scholar curated vocabulary list, or choose \"Blank Plot\" to start with an empty graph.",
      "Type search terms in each . The ",
      br(),
      p(),
      strong("Measurement:"),
      "Here we are using proportion instead of ENTER"))
  })
  
  
  
  
  
  
}

shinyApp(ui, server)

