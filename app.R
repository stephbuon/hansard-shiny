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

#library(bslib) # for more themes
#library(ggrepel)



vals = reactiveValues(btn = FALSE, text = FALSE)
vals_speaker_comparison = reactiveValues(btn = FALSE, text = FALSE)


search_svo <- function(df, s, v, o) {
  if(s == "" & v == "" & o == "") {
    return(df) } else if (s != "" & (v == "" & o == "")) {
      
      df <- df %>%
        filter(str_detect(to_name, paste0("^", s)))
      
      return(df) } else if (s == "" & v != "" & o == "") { 
        print("take 2")
        
      } else if(s == "" & v == "" & o != ""){
        print("take 3")
      } else if(s != "" & v != "" & o == ""){
        print("take 4")
      } else if(s == "" & v != "" & o != ""){
        print("take 5")
      } else if(s != "" & v == "" & o != ""){
        print("take 6")
      } else if(s != "" & v != "" & o != ""){
        print("take 7")
      } 
}




e1 <- fread("~/projects/hansard-shiny/data/network/edges_test_data.csv")
n1 <- fread("~/projects/hansard-shiny/data/network/nodes_test_data.csv")


e1 <- e1 %>%
  filter(decade == 1850)

n1 <- n1 %>%
  filter(decade == 1850)

########### UI 


counts <- "<h3>Count Totals:</h3>"
str1 <- "<h4>Debates: 173,275</h4>"
str2 <- "<h4>Speakers: X</h4>"
str3 <- "<h4>Sentences: 10,979,009</h4>"



ui <- fluidPage(
  
  tags$head(tags$style(HTML(".shiny-output-error-validation { 
                            color: #696969;
                            text-align: center;
                            font-size: 18px; } "))),
  
  theme = shinytheme("yeti"),
  #theme = bs_theme(bootswatch = "litera"),
  
  navbarPage("The Hansard Parliamentary Debates",

             tabPanel("Introduction",
                      splitLayout(cellWidths = c("75%", "25%"),
                                  cellArgs = list(style = "padding: 6px"),
                                  plotlyOutput("ndbs"),
                                  HTML(paste(counts, str1, str2, str3, sep = '<br/>'))),
                      fluidRow(column(width = 7, 
                                      offset = 1,
                                      br(),
                                      p(),
                                      br(),
                                      p("Tools for mining text can open a window onto politics making what happens in government more transparent to citizens."),
                                      p("This protoype app belongs to a preliminary series of public-facing web apps designed to show the language features of debates. 
                                      Users can apply an array of data-mining and statistical tools to gain insight into the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts."),
                                      strong("Controls:"),
                                      "Use the navigation bar at the top of the page to change between different views of the Hansard corpus.",
                                      br(),
                                      p("Click on the blue button found in the top left corner of the following pages to learn more about the data and the methods of measurement used to produce a visualization."),
                                      p(),
                                      p()
                                      ))),

             
             tabPanel("Triples Network", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Check the boxes to explore the langauge of different speakers."),
                          
                          checkboxGroupInput("subreddit", "Speaker:",
                                             c("William E. Gladstone" = "Mr. Gladstone",
                                               "Benjamin Disraeli" = "bdisraeli",
                                               "Arthur Balfour" = "abalfour",
                                               "Mr. Placeholder" = "Mr. Placeholder"),
                                             selected = c("Mr. Gladstone", "Mr. Placeholder")),
                          
                          tags$hr(style="border-color: black;"),
                          helpText("Slide the dial to change decades."),
                          
                          sliderTextInput(
                            inputId = "decade", 
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
                          
                          tags$hr(style="border-color: black;"),
                          helpText("Use the search boxes to filter for triples that contain parts-of-speech."),
                          
                          textInput("search_subject", "Subject:", ""),
                          textInput("search_verb", "Verb:", ""),
                          textInput("search_object", "Object:", ""),
                          actionButton('download_network', 
                                       "Download Plot",
                                       style = "width: 179px;"
                          ),
                          
                          
                          width = 2),
                        
                        mainPanel(visNetworkOutput("network"),
                                  DTOutput('tbl'),
                                  
                                  
                                  ))),

             
             navbarMenu("Nations",
                        tabPanel("Nation Concerns",
                                 sidebarLayout(
                                   sidebarPanel(#style =
                                    # "height: 600px;",
                                     width = 2), 
                                   
                                   mainPanel(plotlyOutput("treemap")))),
                        
                        
                        tabPanel("Nation Pairs",
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
                                     
                                     sliderTextInput(
                                       inputId = "decade_2", 
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
                                     
                                     width = 2), 
                                   
                                   mainPanel(
                                     plotlyOutput("plot_2"),
                                     
                                     
                                     
                                     plotlyOutput("ut")
                                     
                                   
                                     
                                     )))),
             
             
             tabPanel("Collocates",
                      
                      #  splitLayout(cellWidths = c("95%", "5%"),
                      #              cellArgs = list(style = "padding: 6px"),
                      
                      
                      
                      
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("about_collocates", 
                                       "About This Page",
                                       style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
                          p(),
                          #helpText("Choose a vocabulary list."),
                          radioButtons("special_vocabulary", 
                                       "Special Vocabulary:",
                                       c("Property" = "property",
                                         "Concerns" = "concerns",
                                         "Offices" = "offices",
                                         "Nations" = "nations",
                                         "Cities" = "cities")),
                          
                          #tags$hr(style="border-color: black;"),
                          #helpText("Slide the dial to change decades."),
                          sliderTextInput(
                            inputId = "decade_collocates_top", 
                            label = "Decade (Top): ", 
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
                          
                          sliderTextInput(
                            inputId = "decade_collocates_bottom", 
                            label = "Decade (Bottom): ", 
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
                                        "1890"),
                            selected = "1840"),
                          
                          selectInput("noun", 
                                      "Keyword:",
                                      choices = NULL),
                          
                          
                          #tags$hr(style="border-color: black;"),
                          
                          sliderTextInput(
                            inputId = "sentiment", 
                            label = "Sentiment: ", 
                            grid = TRUE, 
                            force_edges = TRUE,
                            choices = c("All",
                                        "Positive", 
                                        "Negative")),
                          
                          radioButtons("collocate_measure", "Measure:", 
                                       list("count", "tf-idf", "jsd"), inline = TRUE, selected = "count"),
                          
                          
                          width = 2),
                        
                        mainPanel(plotlyOutput("collocates_top"),
                                  plotlyOutput("collocates_bottom")))),
             
             navbarMenu("Speakers",
                        tabPanel("Top Speakers",
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("about_top_speakers_by_year", 
                                                  "About This Page",
                                                  style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
                                     p(),
                                     radioButtons("top_speakers_df", "Top Speakers:",
                                                  c("By Year" = "by_year",
                                                    "By Decade" = "by_decade")),
                                     width =2),
                                   
                                   mainPanel(plotlyOutput("top_speakers"),
                                             br(),
                                             br(),
                                             
                                            
                                             plotlyOutput("tbl4",
                                                          height = "580")))),
                        
                        tabPanel("Longest Speeches",
                                 mainPanel(plotlyOutput("longest_speeches"),
                                           dataTableOutput('tbl5'))),
                        
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
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("about_debate_titles", 
                                                  "About This Page",
                                                  style="color: #fff; 
                                                  background-color: #337ab7; 
                                                  border-color: #2e6da4; 
                                                  width: 179px;
                                                  padding:4px; 
                                                  font-size:90%"),
                                     p(),
                                     selectInput("kw_list", 
                                                 "Keyword List:",
                                                 list(`Special Vocabulary` = list("Property" = "property",
                                                                                  "Transportation" = "transportation",
                                                                                  "Resources" = "resources",
                                                                                  "Industry" = "industry"),
                                                      `Custom Vocabulary` = list("Blank Plot" = "custom"))),
                                     
                                     
                                     
                                     textInput("keyword_addition", "Search Terms:", ""),
                                     textInput("keyword_addition_word_2", "", ""),
                                     textInput("keyword_addition_word_3", "", ""),
                                     textInput("keyword_addition_word_4", "", ""),
                                     textInput("keyword_addition_word_5", "", ""),
                                     textInput("keyword_addition_word_6", "", ""),
                                     actionButton('download', 
                                                  "Download Plot",
                                                  style = "width: 179px;"
                                                  ),
                                     width = 2),
                                   
                                   mainPanel(plotlyOutput("debate_titles"),
                                             
                                             tags$script('document.getElementById("download").onclick = function() {
                                             var gd = document.getElementById("debate_titles");
                                             Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                                             var a = window.document.createElement("a");
                                             a.href = url; 
                                             a.type = "image/png";
                                             a.download = "plot.png";
                                             document.body.appendChild(a);
                                             a.click();
                                             document.body.removeChild(a); }); }'),
                                            
                                             br(),
                                             br(),
                                             DTOutput('debate_titles_DT')))),
                        #mainPanel(div(plotlyOutput("debate_titles", height = "100%"), align = "center"))))),
                        tabPanel("Longest Debates",
                                 
                           
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     width =2),
                                   
                                   #fluidRow(column(width = 8, offset = 0,
                                   #                 div(plotlyOutput("longest_debates"), align = "center"),
                                   #                 dataTableOutput('tbl6'))))),
                                   mainPanel(plotlyOutput("longest_debates"),
                                             br(),
                                             br(),
                                            # htmlOutput('tbl6'),
                                             plotOutput('tbl99'))))),
                                            # dataTableOutput('tbl6'))))),
             
             
             navbarMenu("Context",
                        tabPanel("Try 1",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Say something meaningful"),
                                     width = 2),
                                   mainPanel(plotlyOutput("word_embeddings")))),
                        
                        tabPanel("Similarity", # ADD TEXT
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("about_we_similarity", 
                                                  "About This Page",
                                                  style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
                                     p(),
                                     textInput("search_similarity", "Keyword:", value = "harvest"),
                                     actionButton('download_similarity', 
                                                  "Download Plot",
                                                  style = "width: 179px;"
                                     ),
                                     width = 2),
                                  # mainPanel(plotOutput("most_similar"))
                                   mainPanel(plotlyOutput("most_similar"),
                                             
                                             tags$script('
              document.getElementById("download_similarity").onclick = function() {
              var gd = document.getElementById("most_similar");
              Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
                var a = window.document.createElement("a");
                a.href = url; 
                a.type = "image/png";
                a.download = "plot.png";
                document.body.appendChild(a);
                a.click();
                document.body.removeChild(a);                      
              });
              }
              ')
                                             
                                             
                                             
                                             )
                                 )),
                        
                        tabPanel("Try 2",
                         sidebarLayout(
                           sidebarPanel(
                             actionButton("about_try_2", 
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
                                 fluidPage(
                                   fluidRow(
                                     column(10,
                                            offset = 1, 
                                            h3("The Hansard Viewer"),
                                            br(),
                                            p(),
                                            "Tools for mining text can open a window onto politics making what happens in government more transparent to citizens.",
                                            p(),
                                            "For seven years Democracy Lab has been operating at the juncture of political, historical, and textual analysis of democratic debates, publishing numerous articles and collaborating with the builders of infrastructure to make text mining accessible to the public.
                                            This app belongs to a series of preliminary public-facing web applications 
                                            Users of our prototype application can use an array of data-mining and statistical tools to explore the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts. 
                                            Citizens using our toolset can navigate from an overview showing change over time, to a depiction of how different candidates talk about the same issue, to the in-text mentions of word use that a computer has used to produce the visualizations in question.",
                                            p(),
                                            "This data-informed angle into the Hansard debates enables users to perceive change over time at scale. A quantitative approach emphasizes trends, enter. 
                                            
                                            Despite -- it can still show unique occurances in the form of statistical outliers. 
                                            
                                            The usefulness of any application for citizens to understand democracy depends upon how much they can trust the data and its analysis. 
                                            For users to trust the analysis, an app must cut a line between transparency, precision, and innovation. 
                                            Our tool does this by employing methods such as word counts, tf-idf measures, topic models, word embeddings, and triples analysis. 
                                            
                                            
                                            
                                            In general, our app’s design will try to compensate for user mistrust of algorithm- and algebra- dependent measures by linking visualizations to the citations that explain each algorithm or measure and links in lay language. We will also privilege certain measures -- for instance triples analysis, a tool that is simultaneously transparent, precise, and sensitive.")
                                            ) ) ),
                        
                        tabPanel("Code", 
                                 fluidPage(
                                   fluidRow(
                                     column(10,
                                            offset = 1,
                                            h3("Code"),
                                            br(), 
                                            p(),
                                            "Our project values transparency, precision, and innovation. 
                                 
                                 
                                            All of our code is open source and can be found on our",
                                            HTML(" <a href='https://github.com/stephbuon/hansard-shiny'>hansard-shiny</a> GitHub repository."),
                                            #HTML("<ul><li>Transparency</li><li>...more text...</li></ul>"),
                                            "Transparency.",
                                            br(),
                                            p(),
                                            "For the source code, enter"),
                                     ) ) ),
                        
                        tabPanel("Data",
                                 fluidPage(
                                   fluidRow(
                                     column(10,
                                            offset = 1,
                                            h3("Data"),
                                            br(),
                                            p(),
                                            "This page offers links to download the data used by this app.",
                                            br(),
                                            p(),
                                            strong("Hansard:"),
                                            "SMU version of the Hansard data with improved speaker names.",
                                            br(),
                                            p(),
                                            strong("Nations:"),
                                            "placeholder description",
                                            br(),
                                            p(),
                                            strong("Stop Words:"),
                                            "")
                                     ) ) ) ) ) )


server <- function(input, output, session) {
  
  ################## Introduction 
  
  number_of_debates_from_1803_1910 <- fread("~/projects/hansard-shiny/data/introduction/number_of_debates_from_1803_1910.csv")
  
  output$ndbs <- renderPlotly({
    
    plot_ly(data=number_of_debates_from_1803_1910, 
            x = ~decade, 
            y = ~no_of_debates, 
            type = 'bar', 
            text = ~paste0("Decade: ", "<b>", decade, "</b>", "\n",
                           "Number of Debates: ", "<b>", no_of_debates, "</b>", "\n"),
            hoverinfo = "text",
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% 
      layout(title = paste0("The Hansard Parliamentary Debates", "\n", "Debate Count by Decade: 1803—1910"),
             xaxis = list(title = ""),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
    
  })
  
  
  #################### Network ###########################################################################################
  ########################################################################################################################
  
  
  
  #observeEvent(input$download_network, {
    
  #  webshot("network",delay=0.5,zoom=2,file=paste0("network",".png"),
  #          vwidth=900,vheight=900)
    
    #visExport(output$network(),
    #  type = "png",
    #  name = "network_download",
    #  label = paste0("Export as ", type),
    #  background = "#fff")
  #})
  
  
  
  reactive_nodes <- reactive({
    
    e2 <- e1 %>%
      filter(decade == input$decade)
    
    e2 <- e2 %>%
      filter(from_name %in% input$subreddit)
    
    e2 <- search_svo(e2, input$search_subject, input$search_verb, input$search_object)
    
    # cast these edges to a set
    edges_from_list <- e2$from_name
    edges_to_list <- e2$to_name
    total <- c(edges_from_list, edges_to_list)
    total <- unique(total)
    
    # filter for nodes related to an edge
    out <- data.table() 
    for(i in 1:length(total)) {
      
      keyword <- total[i]
      
      filtered_nodes <- n1 %>%
        filter(decade == input$decade) %>%
        filter(str_detect(label, keyword)) #regex(paste0("^", keyword, "$", ignore_case = TRUE))))
      
      out <- rbind(out, filtered_nodes) }
    
    # count the number of times the word was mentioned 
    c <- e2 %>%
      count(to_name) %>%
      rename(value = n,
             label = to_name)
    
    
    out <- left_join(out, c, by = 'label')
    
    out <- unique(out)})
  
  
  reactive_edges <- reactive({
    e3 <- e1 %>%
      filter(from_name %in% input$subreddit) %>%
      add_count(to_name, from_name) %>%
      rename(weight = n) })
  
  
  output$network <- renderVisNetwork({
    
    visNetwork(reactive_nodes(), # 
               reactive_edges(),  # e1
               height = "500px", 
               width = "100%") %>%
      visOptions(#selectedBy = "group", # great for debugging
        #nodesIdSelection = TRUE, # great for debugging
        highlightNearest = list(hover = TRUE)) %>%
      visLayout(randomSeed = 2) %>%
      visPhysics(stabilization = TRUE) %>%
      visEdges(smooth = FALSE,
               length = 210) %>%
      #visNodes(color = list(hover = "green")) %>%
      visInteraction(#hover = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = FALSE) %>%
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -100)) %>%
      
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_selection', nodes.nodes);
                ;}") 
    
    })
  
  observe({ # is this function speeding things up? 
    visNetworkProxy("network") })
  
  
  output$tbl <- renderDT({
    
    validate(need(!is.null(input$current_node_selection), "Click on a node to see its context"))
      
      # e1 <- e1 %>%
      #    filter(from_name %in% input$subreddit | to_name %in% input$subreddit)
      
      #print("currentNodeSection")
      #print(input$current_node_selection)
      
      # This dt might not reflect the correct count -- I need that count function 
     
    datatable(e1 <- e1 %>% 
                filter(from %in% input$current_node_selection | to %in% input$current_node_selection) %>%
                select(-from, -to, -decade),
              options = list(dom = 'ip'),
              filter = list(position = "top"))

    
      # I could return KWIC-like text

      }) 

  
  #################### Nations ###########################################################################################
  ########################################################################################################################
  
  #################### Nation Pairs 

  nations_count <- fread("~/projects/hansard-shiny/data/nations/hansard_c19_debate_title_nation_count.csv")
  
  nations_count$decade <- as.character(nations_count$decade)
  
  nation_pairs <- fread("~/projects/hansard-shiny/data/nations/nation_pairs_in_debate_titles.csv")
  
  output$plot_2 <- renderPlotly({
    
    nation_pairs <- nation_pairs %>%
      filter(decade == input$decade_2) %>%
      arrange(desc(n)) %>%
      slice(1:20)
    
    render_value_np(nation_pairs)
    
    plot_ly(data = nation_pairs, 
            x = ~n, 
            y = ~reorder(nation_pair, n),
            type = 'bar',
            text = ~paste0("Nation Pair: ", "<b>", nation_pair, "</b>", "\n",
                           "Frequency: ", "<b>", n, "</b>", "\n"),
            hoverinfo = "text",
            orientation = 'h',
            source = "np",
            marker= list(color = 'rgb(158,202,225)',
                         line = list(color = 'rgb(8,48,107)',
                                     width = 1.5))) %>% 
      layout(xaxis = list(title ="Frequency"),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F) })
  
  
  
  render_value_np = function(NN){
    output$ut <- renderPlotly({
      s <- event_data("plotly_click", source = "np")
      
      validate(need(!is.null(s), "Click on a bar to visualize each nation's count over time"))
      
      hh <- separate(s, y, into = c("left_nation", "right_nation"), sep = "-")
      
      ln <- nations_count %>%
        filter(debate == hh$left_nation) %>%
        rename(lcount = n)
        
      ln$debate <- "left_nation_1"
        
        ln <- dcast(ln, decade ~ debate)
        
        rn <- nations_count %>%
          filter(debate == hh$right_nation) %>%
          rename(rcount = n)
        
        rn$debate <- "right_nation_1"
        
        rn <- dcast(rn, decade ~ debate)
   
        all <- left_join(ln, rn, on = "decade")
  
        plot_ly(all, 
                x = ~decade, 
                y = ~left_nation_1, 
                type = 'bar',
                marker= list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)), 
                name = hh$left_nation) %>% 
          add_trace(y = ~right_nation_1, 
                    marker= list(color = 'rgb(58,200,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)),
                    name = hh$right_nation) %>% 
          layout(yaxis = list(title = 'Count'), barmode = 'group') %>%
          config(displayModeBar = F)
        }) } 
  
  
  
  #################### Speakers ##########################################################################################
  ########################################################################################################################
  
  #################### Top Words
  
  speaker_favorite_words_count <- fread("~/projects/hansard-shiny/data/speakers/clean_speaker_favorite_words_by_decade.csv")
  
  #layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  #gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  #gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  #gg
  #}
  

  output$top_speakers <- renderPlotly({ 

    top_speakers <- fread(paste0("~/projects/hansard-shiny/data/speakers/top_speakers_", input$top_speakers_df, ".csv"))
    
    render_value(top_speakers)
    
    plot_ly(data = top_speakers, 
            x = ~speechdate, 
            y = ~words_per_day,
            source = "subset",
            type = "scatter",
            marker = list(
              color = 'LightSkyBlue',
              size = 7,
              opacity = 0.8,
              line = list(
                color = 'black',
                width = 1 )),
            text = ~paste0('Name: ', "<b>", speaker, "</b>", '\n',
                           'Speech Date: ', "<b>", speechdate, "</b>", '\n',
                           'Word Count ', "<b>", words_per_day, "</b>"),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Word Count")) %>%
      config(displayModeBar = F) })
  
  render_value = function(NN){
    output$tbl4 <- renderPlotly({#renderPlot({
      s <- event_data("plotly_click", source = "subset")
      
      validate(need(!is.null(s), "Click on a point to view the speaker's top words over time"))
      
      NN <- NN[NN$words_per_day==s$y,]
      
      # read in this df first 
      speaker_favorite_words_count <- speaker_favorite_words_count %>%
        filter(speaker == NN$speaker)
      
      g <- ggplot(data = speaker_favorite_words_count) +
        geom_col(aes(x = reorder_within(word, n, decade), 
                     y = n),
                 fill = "skyblue3",
                 color = "black",
                 size = .3) +
        #fill = "steel blue") +
        labs(title = paste0(NN$speaker, "'s Top Words Over Time"),
             subtitle = "",
             x = "Word",
             y = "Count") +
        #theme(plot.title=element_text(hjust=0.5)) + # doesn't seem to be working
        scale_x_reordered() +
        facet_wrap(~ decade, scales = "free") + 
        coord_flip() + 
        theme_bw() + 
        theme(panel.spacing.y = unit(1, "lines"))
      
      #  make plots same size : https://stackoverflow.com/questions/45696723/how-can-i-make-plotly-subplots-the-same-size-when-converting-from-facets-with-gg
      ggplotly(g) %>%
        #layout_ggplotly %>%
        config(displayModeBar = F) }
  ) }  
  
  
  longest_speeches <- fread("~/projects/hansard-shiny/data/speakers/longest_speeches.csv")
  output$longest_speeches <- renderPlotly({ 
    
    render_value_2(longest_speeches)
    
    plot_ly(data = longest_speeches, 
            x = ~speechdate, 
            y = ~count_per_speech,
            source = "subset_2",
            type = "scatter",
            marker = list(
              color = 'LightSkyBlue',
              size = 7,
              opacity = 0.8,
              line = list(
                color = 'black',
                width = 1 )),
            text = ~paste0('Name: ', "<b>", speaker, "</b>", '\n',
                           'Speech Date: ', "<b>", speechdate, "</b>", '\n',
                           'Speech Word Count ', "<b>", count_per_speech, "</b>"),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Speech Word Count")) %>%
      config(displayModeBar = F) })
  
  render_value_2 = function(NN){
    output$tbl5 <- renderDataTable({
      s <- event_data("plotly_click", source = "subset_2")
      
      validate(need(!is.null(s), "Click on a point to see PLACEHOLDER"))

      return(datatable(NN[NN$count_per_speech==s$y,],
                       options = list(dom = 'ip'),
                       filter = list(position = "top"))) 
      }) } 
  
  
  
  # COME BACK 
  output$speech_lengths <- renderPlotly({
    speech_stats <- fread("~/projects/hansard-shiny/data/speakers/speech_stats.csv")
    
    plot_ly(speech_stats, 
            x = ~decade, 
            y = ~median, 
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(158,202,225)',
                        width = 4))  })
  
  
  
  
  viz <- fread("~/projects/hansard-shiny/data/speakers/speech_lengths_overview.csv")
  hansard_speech_lengths <- fread("~/projects/hansard-shiny/data/speakers/speech_lengths.csv") #%>%
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
  h <- fread("~/projects/hansard-shiny/data/speakers/speaker_comparison_speaker_count_for_app_2.csv")
  j <- fread("~/projects/hansard-shiny/clean_clean_tokenized_hansard_counts.csv", key = "decade")
  
  observeEvent(input$sc_radio_buttons_top,{
    vals_speaker_comparison$btn = TRUE
    vals_speaker_comparison$text = FALSE })
  
  observeEvent(input$sc_custom_search_top,{
    vals_speaker_comparison$btn = FALSE
    vals_speaker_comparison$text = TRUE })

  observeEvent(input$sc_radio_buttons_top,{
    bottom_vals_speaker_comparison$btn = TRUE
    bottom_vals_speaker_comparison$text = FALSE })
  
  observeEvent(input$sc_custom_search_top,{
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
  
  
  
  
  # button, button
  # text, button
  # button, text
  # text, text 
  
  output$speaker_comparison_top <- renderPlotly({
    
    if(vals_speaker_comparison$btn) {
      j <- h 
      j <- j  %>%
        filter(decade == input$sc_decade)
      
      if (input$sc_compare == "sc_top_words") {
        xlab <- list(title ="Frequency") } 
      else if (input$sc_compare == "sc_tf-idf") {
        j <- tf_idf_b(j, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) 
        xlab <- list(title ="tf-idf") } 
      
      j <- j %>%
        filter(clean_new_speaker == input$sc_radio_buttons_top) }
    
    else if (vals_speaker_comparison$text != "") {
      jj <- j[.(as.numeric(input$sc_decade))] # .09 seconds 
      
      setkey(jj, clean_new_speaker)
      j <- jj[.(as.character(input$sc_custom_search_top))]

      if (input$sc_compare == "sc_top_words") {
        xlab <- list(title ="Frequency") } 
      else if (input$sc_compare == "sc_tf-idf") {
        j <- tf_idf_b(jj, input$sc_custom_search_top, input$sc_radio_buttons_bottom)
        xlab <- list(title ="tf-idf")
   
      }
    }
    

    
    if(vals_speaker_comparison$btn) { 
      
      j <- as.data.frame(j)
      
      top <- j %>%
        arrange(desc(n)) %>%
        slice(1:20) 
      
      print(top)
      
      } else {
          
          
          j <- j[order(j, -n)]
          top <- j[1:20]  }
    
    
    if(vals_speaker_comparison$btn){
      ff <- input$sc_radio_buttons_top
    } else {
      ff <- input$sc_custom_search_top
    }
    

    plot_ly(top, 
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

  
  
  
  output$speaker_comparison_bottom <- renderPlotly({
    
    h <- h %>%
      filter(decade == input$sc_decade) 
    
    if (input$sc_compare == "sc_top_words") {
      xlab <- list(title ="Frequency") } 
    
    else if (input$sc_compare == "sc_tf-idf") {
      if(vals_speaker_comparison$btn) {
        h <- tf_idf_b(h, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) 
        xlab <- list(title ="tf-idf") } 
      else {
        h <- tf_idf_b(j, input$sc_custom_search_top, input$sc_radio_buttons_bottom) 
        xlab <- list(title ="tf-idf") } } 
    
    h <- h %>%
      filter(clean_new_speaker == input$sc_radio_buttons_bottom)

    
    top <- h %>%
      arrange(desc(n)) %>%
      slice(1:20)

    plot_ly(top, 
            x = ~n, 
            y = ~reorder(ngrams, n), 
            type = 'bar', 
            text = n,
            orientation = "h",
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(xaxis = xlab,
             yaxis = list(title = "")) %>%
      config(displayModeBar = F) 
  })
  
  
  
  
  

  
  #################### Debates ##########################################################################################
  ########################################################################################################################
  
  
  #################### Debate Titles
  
  words_per_year <- fread("~/projects/hansard-shiny/data/debates/words_per_year.csv")
  debate_title_year_counts <- fread("~/projects/hansard-shiny/data/debates/debate_title_year_counts.csv")
  
  import_debate_titles_data <- reactive( {
    
     if (input$kw_list != "custom") {
       kw_data <- fread(paste0("~/projects/hansard-shiny/data/debates/kw_list_", input$kw_list, ".csv")) } 
     else { 
       kw_data <- data.table() } })
  
  keyword_addition <- function(d, input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6){
    
    keywords <- stri_remove_empty(c(input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6), TRUE)
    
    if (length(keywords != 0)) {
      
      #words_per_year <- read_csv("~/projects/hansard-shiny/data/debates/words_per_year.csv")
      #debate_title_year_counts <- read_csv("~/projects/hansard-shiny/debate_title_year_counts.csv")
      
      if (input$kw_list != "custom") { 
        all_year_counts <- fread(paste0("~/projects/hansard-shiny/data/debates/all_year_counts_", input$kw_list, ".csv")) } 
      else {
        all_year_counts <- fread("~/projects/hansard-shiny/data/debates/all_debate_titles_metadata.csv") }
      
      for (keyword in keywords) {
        
        debate_titles_w_keyword <- debate_title_year_counts %>%
          filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b|^", keyword, "\\b|\\b", keyword, "$"), ignore_case = TRUE)))
        
       # year_count <- debate_titles_w_keyword %>%
      #    group_by(year) %>%
      #    summarise(debates_per_year = sum(year_count)) %>% 
      #    mutate(keywords = keyword)
        
        year_count <- debate_titles_w_keyword %>%
          group_by(year) %>%
          summarise(debates_per_year = sum(n)) %>%
          mutate(keywords = keyword)
        
        all_year_counts <- bind_rows(all_year_counts, year_count) }
      
      all_year_counts <- all_year_counts %>%
        left_join(words_per_year, by = "year") %>%
        mutate(proportion = debates_per_year/words_per_year)

      return(all_year_counts) } else {
        return(d)
      }
    
    
  }
  
  
  output$debate_titles <- renderPlotly({
    
    d <- import_debate_titles_data()
    
    d <- keyword_addition(d, input$keyword_addition, input$keyword_addition_word_2, input$keyword_addition_word_3, input$keyword_addition_word_4, input$keyword_addition_word_5, input$keyword_addition_word_6)
    
    test <- stri_remove_empty(c(input$keyword_addition, input$keyword_addition_word_2, input$keyword_addition_word_3, input$keyword_addition_word_4, input$keyword_addition_word_5, input$keyword_addition_word_6), TRUE)

    
    validate(need(length(d) > 0, "Type words in the text boxes to visualize their proportion over time"))
    
    render_value_debate_titles(d, test)
    
    debate_titles_ggplot <- ggplot(data = d) +
      geom_col(aes(x = year, 
                   y = proportion,
                   fill=reorder(keywords, debates_per_year),
                   text = paste0("Keyword: ", "<b>", keywords, "</b>", "\n",
                                 "Proportion: ", "<b>", proportion, "</b>", "\n",
                                 "Year: ", "<b>", year, "</b>", "\n") )) + 
      scale_fill_viridis(discrete = TRUE, option = "C")+
      guides(fill = guide_legend(title = "Keywords")) +
      theme_bw() + 
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(y = "debates per year as proportion", 
           title = "Proportion of Debate Titles That Include Keywords")
    
    
    geom_point(aes(text=sprintf("letter: %s<br>Letter: %s", a, b)))
    
    
    ggplotly(debate_titles_ggplot,
             source = "TEST",
             tooltip = "text") %>%
      config(displayModeBar = F)
    })
  
  
  m <- fread("~/projects/hansard-shiny/data/debates/all_debate_titles_metadata.csv") %>%
    select(debate, year, year_count)
  
  render_value_debate_titles = function(NN, ...){
    output$debate_titles_DT <- renderDT({
      s <- event_data("plotly_click", source = "TEST")
      
      validate(need(!is.null(s), "Click on a bar to view the debates containing a keyword for that year"))
      
      if (input$kw_list != "custom") {
        metadata <- fread(paste0("~/projects/hansard-shiny/data/debates/kw_metadata_", input$kw_list, ".csv")) 
        test_2 <- left_join(metadata, NN, by = c("keywords", "year")) }
      else {
        
        out <- data.table()
               
        for(i in 1:length(...)) {
          
          keyword <- ...[i]

          validate(need(!is.na(keyword), "Click on a bar to view the debates containing a keyword for that year"))
        
          debate_titles_w_keyword <- m %>%
            filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b|^", keyword, "\\b|\\b", keyword, "$"), ignore_case = TRUE))) %>%
            mutate(keywords = keyword)
          
          out <- bind_rows(debate_titles_w_keyword, out)

          
        }
        

        NN <- NN %>%
          select(-year_count)
        
        
        test_2 <- left_join(out, NN, by = c("keywords", "year", "debate"))

        
      } 
  
      
      test_2 <- test_2 %>%
        select(-debates_per_year, -words_per_year, -proportion)
      
      datatable(test_2[test_2$year==s$x,],
                options = list(dom = 'ip'),
                filter = list(position = "top"))
      
  
      }) }
  
  
  ### NExt 
  
  
  longest_debates <- fread("~/projects/hansard-shiny/data/debates/longest_debates.csv")
  zz <- fread("~/projects/hansard-shiny/data/debates/clean_longest_debates_wordcloud.csv")
  
  output$longest_debates <- renderPlotly({ 
    
    render_value_3(longest_debates)
    render_value_4(longest_debates, zz)
    
    plot_ly(data = longest_debates, 
            x = ~speechdate, 
            y = ~words_per_debate,
            type = "scatter",
            source = "subset_3",
            marker = list(
              color = 'LightSkyBlue',
              size = 7,
              opacity = 0.8,
              line = list(
                color = 'black',
                width = 1 )),
            text = ~paste0('Name: ',  "<b>", debate, "</b>", '\n',
                           'Speech Date: ', "<b>", speechdate, "</b>", '\n',
                           'Word Count ', "<b>",words_per_debate, "</b>"),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Word Count")) %>%
      config(displayModeBar = F) })
  
  render_value_3 = function(NN){
    output$tbl6 <- renderUI({
      s <- event_data("plotly_click", source = "subset_3")

      NN <- NN %>%
        filter(words_per_debate == s$y,
               speechdate == s$x) %>%
        select(-decade, -decade_ranking, -year_ranking)

        str1 <- paste(NN$debate)
        str2 <- paste(NN$debate_id)
        str3 <- paste(NN$speechdate)
        str4 <- paste(NN$words_per_debate)
        HTML(paste("<h3>", 
                   "Debate Title:", str1, "<br>",
                   "Debate ID:", str2, "<br>",
                   "Speech Date:", str3, "<br>",
                   "Words Per Debate:", str4))
    }) } 
  
  
  

  render_value_4 <- function(NN, zz) {
    output$tbl99 <- renderPlot({
      s <- event_data("plotly_click", source = "subset_3")
      
      validate(need(!is.null(s), "Click on a point to view the debate's top words."))
      
      NN <- NN %>%
        filter(words_per_debate == s$y,
               speechdate == s$x)
      
      zz <- zz %>%
        filter(debate == NN$debate,
               speechdate == NN$speechdate) 
      
      set.seed(42)
      ggplot(zz, aes(label = lemma, size = token_count)) +
        geom_text_wordcloud_area(rm_outside = TRUE,
                                 shape = "square") +
        scale_size_area(max_size = 25) +
        theme_minimal() +
        ggtitle(paste0("Top words in ", zz$debate)) + 
        theme(plot.title = element_text(hjust = 0.5,
                                        size=22,
                                        vjust= -10))
      
      }) }
  
  c <- fread("~/projects/hansard-shiny/data/nations/treemap_1800.csv")
  output$treemap <- renderPlotly({
    
    a <- c$a
    b <- c$b
    values <- c$d
    
    plot_ly(
      type="treemap",
      labels = a,
      parents = b,
      values = values) %>%
      layout(height = 600, width = 1000, treemapcolorway=c("3CBB75FF","2D708EFF", "FDE725FF", "481567FF", "1F968BFF")) %>%
      config(displayModeBar = F)
    
    })
  
  
  
  
  
  
  
  
  
  # Collocates
  
  
  filter_sentiment <- function(df, sw) {
    if(sw == "All") {
      return(df) }
    df <- df %>%
      filter(sentiment == sw)
    return (df) }
  

  filter_noun <- function(df, nnn) {
    if(nnn == 'All') {
      return(df) } else {
        df <- df %>%
          filter(str_detect(grammatical_collocates, paste0(" ", nnn))) } }
  
  observe({
    if (input$special_vocabulary == "property") {
      updateSelectInput(session = session, 
                        inputId = "noun", 
                        choices = c("Select" = "All", 
                                    "Evict" = "evict",
                                    "Inclosure" = "inclosure",
                                    "Land" = "land$",
                                    "Landhold" = "landhold",
                                    "Landlord" = "landlord",
                                    "Lease" = "lease",
                                    "Lessee" = "lessee",
                                    "Rent" = "rent",
                                    "Tenant" = "tenan")) } 
    
    else if (input$special_vocabulary == "concerns") {
      updateSelectInput(session = session, 
                        inputId = "noun", 
                        choices = c("Select" = "All", 
                                    "Poor" = "poor")) } 
    else if (input$special_vocabulary == "cities") {
      
      
    }
  })
  
  
  tf_idf_a <- function(df, dct, dcb) {
    
    df <- df %>%
      filter(decade == dct | decade == dcb) 
    
    df <- df %>%
      bind_tf_idf(grammatical_collocates, decade, n)
    
    df <- df %>%
      select(-n) %>%
      rename(n = tf_idf) 
    
    return(df)}
  
  
  

  import_collocates_data <- reactive({ 
    collocates_data <- fread(paste0("~/projects/hansard-shiny/data/collocates/", input$special_vocabulary, "_collocates.csv")) })
  
  output$collocates_top <- renderPlotly({
    
    collocates <- import_collocates_data()
    collocates <- filter_sentiment(collocates, input$sentiment)
    collocates <- filter_noun(collocates, input$noun)
    
    if (input$collocate_measure == "count") {
      xlab <- list(title ="Frequency") } 
    else if (input$collocate_measure == "tf-idf") {
      collocates <- tf_idf_a(collocates, input$decade_collocates_top, input$decade_collocates_bottom)
      xlab <- list(title ="tf-idf") }
    
    setkey(collocates, decade) # could intitalize key on reading data 
    collocates <- collocates[.(as.numeric(input$decade_collocates_top))]
    collocates <- collocates[order(collocates, -n)]
    top <- collocates[1:20]
    
    
    #collocates <- collocates %>%
    #  filter(decade == input$decade_collocates_top)
    
    #top <- collocates %>%
    #  arrange(desc(n)) %>%
    #  slice(1:20)
    
    plot_ly(data = top, 
            x = ~n, 
            y = ~reorder(grammatical_collocates, n),
            type = 'bar',
            text = n, 
            orientation = 'h',
            marker= list(color = 'rgb(158,202,225)',
                         line = list(color = 'rgb(8,48,107)',
                                     width = 1.5))) %>% 
      layout(xaxis = xlab,
             yaxis = list(title = "")) %>%
      #tickangle = 330)) %>%
      config(displayModeBar = F) })
  

  
  output$collocates_bottom <- renderPlotly({
    
    collocates <- import_collocates_data()
    collocates <- filter_sentiment(collocates, input$sentiment)
    collocates <- filter_noun(collocates, input$noun) # just added
    
    if (input$collocate_measure == "count") {
      xlab <- list(title ="Frequency") } 
    else if (input$collocate_measure == "tf-idf") {
      collocates <- tf_idf_a(collocates, input$decade_collocates_top, input$decade_collocates_bottom)
      xlab <- list(title ="tf-idf") }
    
    
    collocates <- collocates %>%
      filter(decade == input$decade_collocates_bottom)
    
    collocates <- filter_noun(collocates, input$noun)
    
    collocates <- collocates %>%
      arrange(desc(n)) %>%
      slice(1:20)
    
    plot_ly(data = collocates, 
            x = ~n, 
            y = ~reorder(grammatical_collocates, n),
            type = 'bar',
            text = n, 
            orientation = 'h',
            marker= list(color = 'rgb(158,202,225)',
                         line = list(color = 'rgb(8,48,107)',
                                     width = 1.5))) %>% 
      layout(xaxis = xlab,
             yaxis = list(title = "")) %>%
      #tickangle = 330)) %>%
      config(displayModeBar = F) })
  
  
  
  
  
  
  #################### Word Embeddings ###################################################################################
  ########################################################################################################################
  
  #################### Try 1 
  
  forplot11 <- fread("~/projects/hansard-shiny/data/word_embeddings/for_plot.csv")
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
  
  #################### Similarity
  
  output$most_similar <- renderPlotly({
    #renderPlot({
    
    out <- data.frame()
    
    decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860)
    
    for(d in 1:length(decades)) {
      
      fdecade <- decades[d] 
      
      table <- paste0("~/projects/hansard-shiny/data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
      word_vectors <- as.matrix(read.table(table, as.is = TRUE))
      
      rn <- rownames(word_vectors)
      
      if(input$search_similarity %in% rn) {
       
        kw = word_vectors[input$search_similarity, , drop = F]
        
        
        cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
        
        forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:16])#[2:21])
        
        colnames(forplot)[1] <- "similarity"
        
        forplot$word <- rownames(forplot)
        
        
        forplot <- forplot %>%
          mutate(decade = fdecade)
        
        out <- bind_rows(out, forplot)
        
      }
      
      
    }
    

    #ggplot(data = out,
    #       aes(x=decade,
    #           y=similarity)) + 
    #  geom_text_repel(label = rownames(out)) +
    #  geom_point() 
      
    # })
  

    
    plot_ly(data = out, 
            x = ~decade, 
            y = ~similarity,
            #mode = "markers",
            mode = "markers+text",
            text = ~word,
            type = "scatter",
            marker = list(color = 'rgb(158,202,225)',
                          size = 15,
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)),
            textposition = "center right",
            height=650) %>%
      config(displayModeBar = F) #%>%
     # add_annotations(x = out$decade,
    #                  y = out$similarity,
    #                  text = rownames(out),
    #                  xref = "x",
    #                  yref = "y",
    #                  showarrow = TRUE,
    #                  arrowhead = 4,
    #                  arrowsize = .5,
    #                  ax = 20,
    #                  ay = -40)
    
    
    })
  
  
  
  
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
      
      table <- paste0("~/projects/hansard-shiny/data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
      word_vectors <- as.matrix(read.table(table, as.is = TRUE))
      
      rn <- rownames(word_vectors)
      if(input %in% rn) {
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
      
      table <- paste0("~/projects/hansard-shiny/data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", fdecade, ".txt")
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
                            dtick = 10),
               #yaxis = list(rangemode = "tozero"),
               title = paste0("Relationship of ", "\"", ff, "\"", " to ", "\"", input$wv_textbox, "\"", " over time")) %>%
        config(displayModeBar = F) } else {
          
          df <- data.frame(decade=integer(),
                           similarity=integer())
          
          plot_ly(data = df,
                  x = ~decade,
                  y = ~similarity) %>%
            config(displayModeBar = F) } })
  

  
  
  #################### Modal Dialog ######################################################################################
  ########################################################################################################################
  
  
  observeEvent(input$about_top_speakers_by_year, {
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
  
  
  observeEvent(input$about_debate_titles, {
    showModal(modalDialog(
      title = "Proportion of Debate Titles that Include Keywords",
      "The number of recorded debates grew significantly over the course of the 19th-century.
      Therefore, we use proportion to measure which subjects were given more attention over time.
      Using proportion adjusts the results for the possibility that any subject might be addressed more often because the number of debates increased, 
      not because more time was spent on one subject relative to another.",
      br(),
      p(),
      strong("Controls:"),
      "Use the \"Keywords List\" drop down box to select a scholar curated vocabulary list, or choose \"Blank Plot\" to start your analysis with an empty plot",
      br(),
      p("Type search terms into the text boxes to add them to the plot.
      The search terms can be one or more words."),
      p("Click on a column in the plot to view the debate titles containing keywords for that for that year."),
      p("With the debate viewer open, use the search box to find debates containing terms, and the the buttons below the debate viewer to navigate through the results."),
      p(),
      strong("Measurement:"),
      "Proportion is found by"))
  })
  
  
  observeEvent(input$about_nation_pairs, {
    showModal(modalDialog(
      title = "Count of Nations and Nation Pairs",
      "Define",
      br(),
      p(),
      strong("Controls:"),
      "Slide the dial under \"Decade\" to change time periods",
      "Click on a bar to visualize the raw count for each nation in the pair over the course of the 19th-century.",
      br(),
      p(),
      strong("Measurement:"),
      "Raw count" ))
  })
  
  
  observeEvent(input$about_try_2, {
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
  
  
  observeEvent(input$about_we_similarity, {
    showModal(modalDialog(
      title = "",
      "DEFINE",
      br(),
      p(),
      strong("Controls:"),
      "Type a single word into the search box to view its most closely associated words throughout the period.",
      br(),
      p(),
      strong("Measurement:"),
      "" ))
  })
  
  observeEvent(input$about_collocates, {
    showModal(modalDialog(
      title = "Special Vocabulary: Sentiment Laden Collocates",
      "A collocate is a series of words that co-occur in text. 
        A grammatical collocate represents the co-occuring words that share a sentence-level grammatical relationship.",
      br(),
      p(),
      strong("Controls:"),
      "Click a radio button under \"Special Vocabulary\" to select a scholar curated vocabulary list to guide your search.",
      br(),
      p(),
      "Slide the dials under \"Decade\" to change time periods",
      br(),
      p(),
      "Choose a keyword from the drop down box to narrow your analysis to just collocates containing that word.
        The keywords will update based on the selected vocabulary list.",
      br(),
      p(),
      "Slide the dial under \"Sentiment\" to narrow your analysis from all sentiment laden collocates to just those with positive or negative scores.",
      br(),
      p(),
      "Click a radio button under \"Measure\" to return results based on: 
        a) a frequency count; b) term frequency - inverse document frequency (tf-idf); or c) Jenson-Shannon divergence (jsd).",
      br(),
      p(),
      strong("Measurements:"),
      "\"Count\" refers to the number of times a pair of collocates appeared in a sentence in the debate text.",
      br(),
      p(),
      "\"tf-idf\" is a numerical statistic that reflects how \"important\" a word is to a corpus. 
        The tf–idf value increases proportionally to the number of times a word appears in a decade and is offset by the other decade that contains the word, 
        which helps to adjust the results for the fact that some words appear more frequently in general.",
      br(),
      p(),
      "\"jsd\" is a " ))
    })
  
  
}

shinyApp(ui, server)

