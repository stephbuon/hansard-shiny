# remember that rN the network data is in network/cleaning.R



# remember to add cache stuff 
# If I want to see the full network, I can pass VisNetwork the static edges data (e1) instead of the reactive expression (reactive_edges()). 

# keep from recalculating: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

# Q for when I run static data: why does graph get smaller with more nodes? I think bc I am then selecting nodes with, say, London AND Lymric. Not 100% sure.

# https://docs.google.com/document/d/1DN5xy1WlA_nqW0xUtG0ErJbhqSfuURB3G5HSr9Swnxc/edit

# connections is the number of word pairs in which the node appears
# total weight -- IDK

# google drive: https://docs.google.com/document/d/15rDOVGFlx8JF7DmysuqGS02Be7air8wka_J6dxL4k9I/edit


# I am working on an interactive mining app
# using triples to visualize descriptions of the "other" in covid data 
# I don't keep ambig like saint johns nation or church

# add favorite words for speaker -- click on point and return DF with that info 

# nations-concerns -- would return a histogram for both nations showing top concerns

# triples network could instead be nations 

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(visNetwork)
library(viridis)
library(plotly)
library(tidyverse)
library(DT)
library(tidytext)
library(stringi)
library(reshape2)
library(text2vec)

library(shinyBS)

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

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}



directions <- data.frame("To explore text click on a node. Results will appear here.") 
names(directions)[1] <- " "


###########################



e1 <- read_csv("~/projects/hansard-shiny/edges_test_data.csv")
n1 <- read_csv("~/projects/hansard-shiny/nodes_test_data.csv")





######################

counts <- "<h3>Count Totals:</h3>"
str1 <- "<h4>Debates: 173,275</h4>"
str2 <- "<h4>Speakers: X</h4>"
str3 <- "<h4>Sentences: 10,979,009</h4>"


ui <- fluidPage(
  #theme = shinytheme("flatly"),
  theme = shinytheme("cosmo"),
  navbarPage("The Hansard Parliamentary Debates",
             
             tabPanel("Introduction",
                      splitLayout(cellWidths = c("75%", "25%"),
                                  cellArgs = list(style = "padding: 6px"),
                                  plotlyOutput("ndbs"),
                                  
                      
                                  #textOutput("text1"))),
                                  
                                  
                                  HTML(paste(counts, str1, str2, str3, sep = '<br/>'))),
                      fluidRow(column(width = 7, 
                                      offset = 1,
                                      br(),
                                      p(),
                                      br(),
                                      p(),
                                      "Tools for mining text can open a window onto politics making what happens in government more transparent to citizens.",
                                      br(),
                                      p(),
                                      "This protoype app belongs to a preliminary series of public-facing web apps designed to show the language features of debates. 
                                      Users can apply an array of data-mining and statistical tools to gain insight into the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts.",
                                      br(),
                                      p(),
                                      strong("Controls:"),
                                      "Use the navigation bar at the top of the page to change between different views of the Hansard corpus.",
                                      br(),
                                      p(),
                                      "Click on the blue button found in the top left corner of the following pages to learn more about the data and the methods of measurement used to produce a visualization."
                                      
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
                          
                          #downloadButton('download_plot', ' Download Plot '),
                          
                          width = 2), # The width of the sidebar panel 
                        
                        mainPanel(visNetworkOutput("network"),
                                  DTOutput('tbl')))),
             
             
             navbarMenu("Nations",
                        tabPanel("Nation Concerns",
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 2), 
                                   
                                   mainPanel(plotlyOutput("treemap")))),
                                 
                        
                        tabPanel("Nation Pairs",
                        #titlePanel("How many times was each nation pair named in Hansard debate titles?"),
                        sidebarLayout(
                          sidebarPanel(
                            actionButton("about_nation_pairs", 
                                         "About This Page",
                                         style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       padding:4px; 
                                       font-size:90%"),
                            p(),
                            helpText("This page visualizes the nation pairs that occured most frequently in debate titles. 
                                     Click on a nation pair to view how the top concern associated with each nation changed over time."),
                          
                            tags$hr(style="border-color: black;"),
                          
                            helpText("Slide the dial to change decades."),
                          
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
                          plotlyOutput("ut"))))),
             
             
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
                          
                          #tags$hr(style="border-color: black;"),
                          
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
                          
                          
                          radioTooltip(id = "collocate_measure", choice = "count", title = "View the raw count.", placement = "right", trigger = "hover"),
                          radioTooltip(id = "collocate_measure", choice = "tf-idf", title = "Use tf-idf to find important words.", placement = "right", trigger = "hover"),
                          radioTooltip(id = "collocate_measure", choice = "jsd", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
                          
                          width = 2),
                       # mainPanel(plotlyOutput("collocates_top"),
                       #           plotlyOutput("collocates_bottom")
                        
                       # )))),
                        
                        mainPanel(plotlyOutput("collocates_top"),
                                  plotlyOutput("collocates_bottom")))),
             
             navbarMenu("Speakers",
                        tabPanel("Top Speakers",
                                 mainPanel(plotlyOutput("top_speakers"),
                                           br(),
                                           br(),
                                           plotlyOutput("tbl4",
                                                        height = "580"))),
                        tabPanel("Longest Speeches",
                                 mainPanel(plotlyOutput("longest_speeches"),
                                           dataTableOutput('tbl5'))),
                        tabPanel("Speech Lengths",
                                 mainPanel())),
             
             # plotlyOutput("speech_lengths")
             
             
             
             navbarMenu("Debates",
                        tabPanel("Debate Titles",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Say something meaningful"),
                                     actionButton("about_debate_titles", 
                                                  "About This Page",
                                                  style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
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
                                     width = 2),
                                   
                                     mainPanel(plotlyOutput("debate_titles"),
                                               DTOutput('debate_titles_DT')))),
                                   #mainPanel(div(plotlyOutput("debate_titles", height = "100%"), align = "center"))))),
                        tabPanel("Longest Debates",
                                 sidebarLayout(helpText("hello"),
                                   
                                 #fluidRow(column(width = 8, offset = 0,
                                #                 div(plotlyOutput("longest_debates"), align = "center"),
                                #                 dataTableOutput('tbl6'))))),
                                 mainPanel(plotlyOutput("longest_debates"),
                                           dataTableOutput('tbl6'))))),

             
             navbarMenu("Context",
                        tabPanel("Try 1",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Say something meaningful"),
                          
                                     width = 2),
                                   mainPanel(plotlyOutput("word_embeddings")))),
                        tabPanel("Similarity",
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Say something meaningful"),
                                     textInput("search_similarity", "Keyword:", ""),
                                     width = 2),
                                   mainPanel(plotlyOutput("most_similar"))
                                   )
                                 )),
             
             
             navbarMenu("About",
                        tabPanel("Purpose",
                                 h3("The Hansard Viewer"),
                                 br(),
                                 p(),
                                 "Tools for mining text can open a window onto politics making what happens in government more transparent to citizens.",
                                 p(),
                                 "For seven years, our lab, Democracy Lab, has been operating at the juncture of political, historical, and textual analysis of democratic debates, publishing numerous articles, collaborating with the builders of infrastructure to make text mining accessible to the public, and building a preliminary series of public-facing apps. 
                                 Users of our prototype web application can use an array of data-mining and statistical approaches which can provide new insights into the evolution and nature of political language as it occurs in different time periods, across different speakers, and in different national contexts. 
                                 Citizens using our toolset can navigate from an overview showing change over time, to a depiction of how different candidates talk about the same issue, to the in-text mentions of word use that a computer has used to produce the visualizations in question.",
                                 p(),
                                 "The usefulness of any application for citizens to understand democracy depends upon how much they can trust the data and its analysis. 
                                 For users to trust the analysis, an app must cut a line between transparency, precision, and innovation. 
                                 Our tool does this by employing methods such as word counts, tf-idf measures, topic models, word embeddings, and triples analysis. 
                                 
                                 
                                 In general, our app’s design will try to compensate for user mistrust of algorithm- and algebra- dependent measures by linking visualizations to the citations that explain each algorithm or measure and links in lay language. We will also privilege certain measures -- for instance triples analysis, a tool that is simultaneously transparent, precise, and sensitive. 

                                 
                                 
                                 "),
                        
                        tabPanel("Code", 
                                 h3("Code"),
                                 br(), 
                                 p(),
                                 "Our code is written with 
                                 
                                 is open source and can be found on our",
                                 HTML(" <a href='https://github.com/stephbuon/hansard-shiny'>hansard-shiny</a> GitHub repository."),
                                 #HTML("<ul><li>Transparency</li><li>...more text...</li></ul>"),
                                 "Transparency.",
                                 br(),
                                 p(),
                                 "For the source code, enter"),
                        tabPanel("Data",
                                 "Placeholder text.",
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
                                 ""))
  ))


server <- function(input, output, session) {
  
  ################## Introduction 
  
  number_of_debates_from_1803_1910 <- read_csv("~/projects/hansard-shiny/data/introduction/number_of_debates_from_1803_1910.csv")
  
  output$ndbs <- renderPlotly({
    
    #render_value_text(number_of_debates_from_1803_1910)
    
    plot_ly(data=number_of_debates_from_1803_1910, 
            x = ~decade, 
            y = ~no_of_debates, 
            type = 'bar', 
            source = "texty",
            text = ~paste0("Decade: ", decade, "\n",
                           "Number of Debates: ", no_of_debates, "\n"),
            hoverinfo = "text",
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% 
      layout(title = paste0("The Hansard Parliamentary Debates", "\n", "Debate Count by Decade: 1803—1910"),
             xaxis = list(title = ""),
             yaxis = list(title = "")) %>%
      config(displayModeBar = F)
    
  })
  
  
  
  
  #text = ~paste0('Name: ', speaker, '\n',
  #               'Speech Date: ', speechdate, '\n',
  #               'Speech Word Count ', count_per_speech),
  #hoverinfo = 'text') %>%
  
  
  
  
  
  
  
  
  #render_value_text = function(NN){
    
   # output$text1 <- renderText({
      #s <- event_data("plotly_click", source = "texty") 
      #print(s)
      str1 <- "Number of Debates: ENTER"
      str2 <- "Number of Speakers: ENTER"
    #  HTML(paste(str1, str2, sep = '<br/>'))
      
     # }) }
  
  
  #######################################################3
  
  
  
  
  
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
    out <- data.frame() 
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
    
    out <- unique(out) 
    })
  
  
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
                ;}") })
  
  observe({ # is this function speeding things up? 
    visNetworkProxy("network") })
  
  
  output$tbl <- renderDT(
    if (!(is.null(input$current_node_selection))) {
      
      # e1 <- e1 %>%
      #    filter(from_name %in% input$subreddit | to_name %in% input$subreddit)
      
      #print("currentNodeSection")
      #print(input$current_node_selection)
      
      # This dt might not reflect the correct count -- I need that count function 
      e1 <- e1 %>% 
        filter(from %in% input$current_node_selection | to %in% input$current_node_selection)# %>%
      #select(!(c("to_name", "from")))
      
      #print("dt test")
      #print(e1)
      
      # I could return KWIC-like text
      
    } else { directions }) 
  
  
  ####################### Nation Pairs
  
  
  

  
  nations_count <- read_csv("~/projects/hansard-shiny/data/nations/hansard_c19_debate_title_nation_count.csv")
  
  nations_count$decade <- as.character(nations_count$decade)
  
  
  
  
  output$plot_2 <- renderPlotly({
    nation_pairs <- read_csv("~/projects/hansard-shiny/data/nations/nation_pairs_in_debate_titles.csv")
    
    nation_pairs <- nation_pairs %>%
      filter(decade == input$decade_2) %>%
      arrange(desc(n)) %>%
      slice(1:20)
    
    render_value_np(nation_pairs)
    
    plot_ly(data = nation_pairs, 
            x = ~n, 
            y = ~reorder(nation_pair, n),
            type = 'bar',
            text = n, 
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
    
    if(!is.null(s)) {
    hh <- separate(s, y, into = c("left", "right"), sep = "-")
    
    print(hh)
    
    left_nation <- hh$left
    right_nation <- hh$right
    
    #left_nation <- str_to_title(left_nation)
    #right_nation <- str_to_title(right_nation)
    
    ln <- nations_count %>%
      filter(debate == left_nation) %>%
      rename(lcount = n)
    
    ln$debate <- "left_nation_1"
    
    ln <- dcast(ln, decade ~ debate)
    
    
    
    rn <- nations_count %>%
      filter(debate == right_nation) %>%
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
                   name = left_nation) %>% 
      add_trace(y = ~right_nation_1, 
                marker= list(color = 'rgb(58,200,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)),
                name = right_nation) %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'group') %>%
      config(displayModeBar = F)
    
    
    }
  }) } 
  
  
  
  
  
  #############################################################
  
  speaker_favorite_words_count <- read_csv("~/projects/hansard-shiny/data/speakers/clean_speaker_favorite_words_by_decade.csv")
  
  #layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
    # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
    #gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
    #gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
    #gg
  #}
  
  #top_speaker_title <- function(input, output) {
  #    output$top_speaker_title <- renderText({
        
   #     paste("You have chosen a range that goes from",
  #                                      input$range[1], "to", input$range[2])})
   # }
  
  output$top_speakers <- renderPlotly({ 
    top_speakers <- read_csv("~/projects/hansard-shiny/data/speakers/top_speakers.csv")
    
    render_value(top_speakers)
    
    plot_ly(data = top_speakers, 
            x = ~speechdate, 
            y = ~words_per_day,
            source = "subset",
            marker = list(
              color = 'LightSkyBlue',
              size = 7,
              opacity = 0.8,
              line = list(
                color = 'black',
                width = 1 )),
            text = ~paste0('Name: ', speaker, '\n',
                           'Speech Date: ', speechdate, '\n',
                           'Word Count ', words_per_day),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Word Count")) %>%
      config(displayModeBar = F) })
  
  render_value = function(NN){
    output$tbl4 <- renderPlotly({#renderPlot({
      s <- event_data("plotly_click", source = "subset")
      #print('NEED THIS')
      #print(s)
      #print(NN)
      #return(datatable(NN[NN$words_per_day==s$y,])) 
      NN <- NN[NN$words_per_day==s$y,]
      
      #print(NN)
      
      aa <- NN$speaker
      
      #print(aa)
      
      # read in this df first 
      speaker_favorite_words_count <- speaker_favorite_words_count %>%
        filter(speaker == aa)
      
      #print(speaker_favorite_words_count)

      g <- ggplot(data = speaker_favorite_words_count) +
        geom_col(aes(x = reorder_within(word, n, decade), 
                     y = n),
                 fill = "skyblue3",
                 color = "black",
                 size = .3) +
                 #fill = "steel blue") +
        labs(title = paste0(aa, "'s Top Words Over Time"),
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
        config(displayModeBar = F)

      
      
    }) } 
  
  
  output$longest_speeches <- renderPlotly({ 
    longest_speeches <- read_csv("~/projects/hansard-shiny/data/speakers/longest_speeches.csv")
    
    render_value_2(longest_speeches)
    
    plot_ly(data = longest_speeches, 
            x = ~speechdate, 
            y = ~count_per_speech,
            source = "subset_2",
            marker = list(
              color = 'LightSkyBlue',
              size = 7,
              opacity = 0.8,
              line = list(
                color = 'black',
                width = 1 )),
            text = ~paste0('Name: ', speaker, '\n',
                           'Speech Date: ', speechdate, '\n',
                           'Speech Word Count ', count_per_speech),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Speech Word Count")) %>%
      config(displayModeBar = F) })
  
  render_value_2 = function(NN){
    output$tbl5 <- renderDataTable({
      s <- event_data("plotly_click", source = "subset_2")
      return(datatable(NN[NN$count_per_speech==s$y,])) 
    }) } 
  
  
  
  # COME BACK 
  output$speech_lengths <- renderPlotly({
    speech_stats <- read_csv("~/projects/hansard-shiny/data/speakers/speech_stats.csv")
    
    plot_ly(speech_stats, 
                           x = ~decade, 
                           y = ~median, 
                           type = 'scatter', 
                           mode = 'lines',
                           line = list(color = 'rgb(158,202,225)',
                                       width = 4))
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ######################### Debates 
  
  output$longest_debates <- renderPlotly({ 
    longest_debates <- read_csv("~/projects/hansard-shiny/data/debates/longest_debates.csv")
    
    render_value_3(longest_debates)
    render_value_4(longest_debates)
    
    plot_ly(data = longest_debates, 
            x = ~speechdate, 
            y = ~words_per_debate,
            source = "subset_3",
            marker = list(
              color = 'LightSkyBlue',
              size = 7,
              opacity = 0.8,
              line = list(
                color = 'black',
                width = 1 )),
            text = ~paste0('Name: ', debate, '\n',
                           'Speech Date: ', speechdate, '\n',
                           'Word Count ', words_per_debate),
            hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Word Count")) %>%
      config(displayModeBar = F) })
  
  render_value_3 = function(NN){
    output$tbl6 <- renderDataTable({
      s <- event_data("plotly_click", source = "subset_3")
      return(datatable(NN[NN$words_per_debate==s$y,])) 
    }) } 
  
  render_value_4 <- function(NN) {
    output$tbl99 <- renderPlotly({
      s <- event_data("plotly_click", source = "subset")
      NN <- NN[NN$token_count==s$y,]
      print(NN)
      
    })
    
    
    
  }
      

  observeEvent(input$about_debate_titles, {
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
      "Here we are using proportion instead of ENTER"
      
    ))
  })
  
  observeEvent(input$about_nation_pairs, {
    showModal(modalDialog(
      title = "Raw Count of Nations and Nation Pairs",
      "Define",
      br(),
      p(),
      strong("Controls:"),
      "Slide the dial under \"Decade\" to change time periods",
      "Click on a bar to visualize the raw count for each nation in the pair over the course of the 19th-century.",
      br(),
      p(),
      strong("Measurement:"),
      "Raw count"
      
    ))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$word_embeddings <- renderPlotly({
    forplot <- read_csv("~/projects/hansard-shiny/data/word_embeddings/for_plot.csv")
    
    plot_ly(data = forplot, 
            x = ~V1, 
            y = ~V2, 
            type = 'scatter', 
            mode = 'text',
            text = ~word) %>% 
      layout(#autosize = F,
        xaxis = list(title = "Placeholder"),
        yaxis = list(title = "Placeholder")) %>%
      config(displayModeBar = F)
    
    
  })
  
  
  
  output$treemap <- renderPlotly({
    
    c <- read_csv("~/projects/hansard-shiny/treemap_1880.csv")
    
    
    a <- c$a
    b <- c$b
    values <- c$d
    
    
    plot_ly(
    type="treemap",
    labels = a,
    parents = b,
    values = values) %>%
      layout(height = 600, width = 1000, treemapcolorway=c("3CBB75FF","2D708EFF", "FDE725FF", "481567FF", "1F968BFF")) %>%
      config(displayModeBar = F)})

  

  
  
  
  
  
  
  # Collocates
  
  filter_sentiment <- function(df, sw){
    if(sw == 'All') {
      return (df) } else if(sw == 'Positive') {
        df <- df %>%
          filter(sentiment == 'Positive') 
        return(df)} else if(sw == 'Negative') {
          df <- df %>%
            filter(sentiment == 'Negative')
          return(df) } }
  
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
  
  
  
  
  
  ##################### collocates
  
  
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
        "\"jsd\" is a "
        
      ))
    })
    
  import_collocates_data <- reactive({
      collocates_data <- read_csv(paste0("~/projects/hansard-shiny/data/collocates/", input$special_vocabulary, "_collocates.csv")) })
  
  
  
  output$collocates_top <- renderPlotly({
    
    collocates <- import_collocates_data() #read_csv("~/projects/hansard-shiny/data/collocates/property_collocates.csv")

    collocates <- filter_sentiment(collocates, input$sentiment)
    
    
    collocates <- filter_noun(collocates, input$noun)
    
     if (input$collocate_measure == "count") {
       xlab <- list(title ="Frequency")
       
     } else if (input$collocate_measure == "tf-idf") {
      
       collocates <- tf_idf_a(collocates, input$decade_collocates_top, input$decade_collocates_bottom)
       
       xlab <- list(title ="tf-idf")
       #write_csv(collocates, "~/debug.csv")
       
     }
    
    collocates <- collocates %>%
      filter(decade == input$decade_collocates_top)
    
    top <- collocates %>%
      arrange(desc(n)) %>%
      slice(1:20)
    
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
    
    if (input$collocate_measure == "count") {
      
      xlab <- list(title ="Frequency")
      
    } else if (input$collocate_measure == "tf-idf") {
      
      collocates <- tf_idf_a(collocates, input$decade_collocates_top, input$decade_collocates_bottom)
      
      xlab <- list(title ="tf-idf")
      #write_csv(collocates, "~/debug.csv")
      
    }
    
    
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
  
  
  
  
  
  
  #################### Word Embeddings
  
  output$word_embeddings <- renderPlotly({
    forplot <- read_csv("~/projects/hansard-shiny/data/word_embeddings/for_plot.csv")
    
    plot_ly(data = forplot, 
            x = ~V1, 
            y = ~V2, 
            type = 'scatter', 
            mode = 'text',
            text = ~word) %>% 
      layout(#autosize = F,
        xaxis = list(title = "Placeholder"),
        yaxis = list(title = "Placeholder")) %>%
      config(displayModeBar = F)
    
    
  })
  
  
  
  #word_vectors <- as.matrix(read.table("~/projects/hansard-shiny/hansard_word_vectors_1800.txt", as.is = TRUE))
  
  output$most_similar <- renderPlotly({
    
    
    out <- data.frame()
    
    decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860)
    
    for(d in 1:length(decades)) {
      
      fdecade <- decades[d] 
      
      table <- paste0("~/projects/hansard-shiny/hansard_word_vectors_", fdecade, ".txt")
      
      word_vectors <- as.matrix(read.table(table, as.is = TRUE))
      
      kw = word_vectors[input$search_similarity, , drop = F]
      
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:21])
      
      colnames(forplot)[1] <- "similarity"
      
      forplot$word <- rownames(forplot)
      
      forplot <- forplot %>%
        mutate(decade = fdecade)
      
      out <- bind_rows(out, forplot)
    }
    
  
    plot_ly(data = out, 
                   x = ~decade, 
                   y = ~jitter(similarity),
                   mode = "markers+text",
                   text = ~word,
                   type = "scatter",
                   marker = list(color = 'rgb(158,202,225)',
                                 size = 15,
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)),
                   textposition = "center right",
                   height=600) %>%
      config(displayModeBar = F) })
  
    
  
  
  #################### Debate Titles
  
  import_debate_titles_data <- reactive( {
    
    if (input$kw_list != "custom") {
      kw_data <- read_csv(paste0("~/projects/hansard-shiny/data/debates/kw_list_", input$kw_list, ".csv")) } 
    else { 
      kw_data <- data.frame() } })
  
  keyword_addition <- function(d, input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6){
    
   keywords <- stri_remove_empty(c(input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6), TRUE)
    
    if (length(keywords != 0)) {
      
      words_per_year <- read_csv("~/projects/hansard-shiny/data/debates/words_per_year.csv")
      debate_title_year_counts <- read_csv("~/projects/hansard-shiny/debate_title_year_counts.csv")
      
      if (input$kw_list != "custom") {
        all_year_counts <- read_csv(paste0("~/projects/hansard-shiny/data/debates/all_year_counts_", input$kw_list, ".csv")) } 
      else {
        all_year_counts <- data.frame()
      }
    
      # if all year counts has data 
      
    for(i in 1:length(keywords)){
      
      keyword <- keywords[i]
      
      debate_titles_w_keyword <- debate_title_year_counts %>%
        filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b"), ignore_case = TRUE)))
      
      # if input keyword does not exist, else: 
      #if (identical(debate_titles_w_keyword, debate_title_year_counts))
      
      year_count <- debate_titles_w_keyword %>%
        group_by(year) %>%
        summarise(debates_per_year = n()) %>%
        arrange(year) %>%
        #mutate(keywords = input_keyword_addition)
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
    
    render_value_debate_titles(d)
    
    debate_titles_ggplot <- ggplot(data = d) +
      geom_col(aes(x = year, 
                   y = proportion,
                   fill=reorder(keywords, debates_per_year))) + 
      scale_fill_viridis(discrete = TRUE, option = "C")+
      guides(fill = guide_legend(title = "Keywords")) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(y = "debates per year as proportion", 
           title = "Proportion of Debate Titles That Include Keywords")
  
  ggplotly(debate_titles_ggplot,
           source = "TEST") %>%
    config(displayModeBar = F)})
  
  
  render_value_debate_titles = function(NN){
    output$debate_titles_DT <- renderDT({
      s <- event_data("plotly_click", source = "TEST")
      
      
      if (input$kw_list != "custom") {
        metadata <- read_csv(paste0("~/projects/hansard-shiny/kw_metadata_", input$kw_list, ".csv")) } 
      else {
        metadata <- data.frame()
      }
      
      print(metadata)
      
      test_2 <- left_join(metadata, NN, by = c("keywords", "year"))
      
      #print(test_2)
     # print(datatable(NN[NN$words_per_day==s$y,])) 
      
      #NN <- NN[NN$words_per_day==s$y,]
      
      #print(NN)
      
      #aa <- NN$speaker
      
      #test_2 <- left_join(all_year_counts, metadata, by = c("keywords", "year"))
      
      
      #print(aa)
      
      # read in this df first 
     # speaker_favorite_words_count <- speaker_favorite_words_count %>%
       # filter(speaker == aa) 
      }) }
    
    
    
  
  
  
  
  
  
}

shinyApp(ui, server)



#layout(#autosize = F,
#   xaxis = xlab,
#   yaxis = ylab,
#  margin = list(l = 160), xaxis = list(tickangle = 45)) %>%

#config(displayModeBar = F)