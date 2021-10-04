# remember to add cache stuff 
# If I want to see the full network, I can pass VisNetwork the static edges data (e1) instead of the reactive expression (reactive_edges()). 

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
library(plotly)
library(tidyverse)
library(DT)
library(tidytext)

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



directions <- data.frame("To explore text click on a node. Results will appear here.") 
names(directions)[1] <- " "


###########################




from <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
to <- c(3, 4, 5, 6, 7, 8, 8, 8, 9, 10, 11)
from_name <- c('Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Mr. Gladstone', 'Dummy Name', 'Dummy Name', 'Dummy Name', 'Dummy Name', 'Dummy Name') 
to_name <- c('corporation-discharge-duty', 'friend-be-willing', 'who-not-see-on-occasion', 'planter-be-commendable', 'what-not-occur-with-respect', 'it-be-in-opinion', 'it-be-in-opinion', 'it-be-in-opinion', 'he-talks-to-members', 'he-vote-on-bill', 'he-return-to-court')
e1 <- data.frame(from_name, to_name, from, to)

id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
group <- c('speaker', 'speaker', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple', 'triple')
label <- c('Mr. Gladstone', 'Dummy Name', 'corporation-discharge-duty', 'friend-be-willing', 'who-not-see-on-occasion', 'planter-be-commendable', 'what-not-occur-with-respect', 'it-be-in-opinion', 'he-talks-to-members', 'he-vote-on-bill', 'he-return-to-court')
decade <- c('1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800', '1800')
n1 <- data.frame(id, group, label)


e1 <- edges
n1 <- nodes




######################




ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage("The Hansard Parliamentary Debates",
             
             tabPanel("Triples Network", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Check the boxes to explore the langauge of each speaker."),
                          
                          checkboxGroupInput("subreddit", "Speaker:",
                                             
                                             c("William E. Gladstone" = "Mr. Gladstone",
                                               "Benjamin Disraeli" = "bdisraeli",
                                               "Arthur Balfour" = "abalfour",
                                               "Name" = "Dummy Name"),
                                             
                                             selected = c("Mr. Gladstone", "Dummy Name")),
                          
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
             
             
             tabPanel("Nations",
                      #titlePanel("How many times was each nation pair named in Hansard debate titles?"),
                      sidebarLayout(
                        sidebarPanel(
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
                          plotlyOutput("plot_2")))),
             
             
             tabPanel("Collocates",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("This page visualizes sentiment laden collocates."),
                          
                          radioButtons("special_vocabulary", 
                                       "Special Vocabulary:",
                                       c("Property" = "property",
                                         "Concerns" = "concerns",
                                         "Offices" = "offices",
                                         "Nations" = "nations",
                                         "Cities" = "cities")),
                          
                          #tags$hr(style="border-color: black;"),
                          
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
                                        "1890")),
                          
                          #tags$hr(style="border-color: black;"),
                          
                          selectInput("noun", 
                                      "Keyword:",
                                      choices = NULL),
                                      #c("Select" = "All",
                                      #  "Evict" = "evict",
                                      #  "Inclosure" = "inclosure",
                                      #  "Land" = "land$",
                                      #  "Landhold" = "landhold",
                                      #  "Landlord" = "landlord",
                                      #  "Lease" = "lease",
                                      #  "Lessee" = "lessee",
                                      #  "Rent" = "rent",
                                      #  "Tenant" = "tenan")),
                          
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
                                 mainPanel(plotlyOutput("top_speakers"),
                                           br(),
                                           br(),
                                           plotlyOutput("tbl4",
                                                        height = "580"))),
                        tabPanel("Longest Speeches",
                                 mainPanel(plotlyOutput("longest_speeches"),
                                           dataTableOutput('tbl5')))),
             
             tabPanel("Debates",
                      mainPanel(plotlyOutput("longest_debates"),
                                dataTableOutput('tbl6'))),
             
             
             tabPanel("Word Embeddings",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Say something meaningful"),
                          
                          width = 2),
                        
                        mainPanel(plotlyOutput("word_embeddings")))),
             
             
             tabPanel("About", 
                      "Placeholder text")
  ))


server <- function(input, output, session) {
  
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
    
    print(out)
    
    # count the number of times the word was mentioned 
    c <- e2 %>%
      count(to_name) %>%
      rename(value = n,
             label = to_name)
    
    
    out <- left_join(out, c, by = 'label')
    
    out <- unique(out) 
    
    print(out) })
  
  
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
      
      print("currentNodeSection")
      print(input$current_node_selection)
      
      # This dt might not reflect the correct count -- I need that count function 
      e1 <- e1 %>% 
        filter(from %in% input$current_node_selection | to %in% input$current_node_selection)# %>%
      #select(!(c("to_name", "from")))
      
      print("dt test")
      print(e1)
      
      # I could return KWIC-like text
      
    } else { directions }) 
  
  
  output$plot_2 <- renderPlotly({
    nation_pairs <- read_csv("~/projects/hansard-shiny/data/nations/clean_nation_pairs_in_titles.csv")
    
    nation_pairs <- nation_pairs %>%
      filter(decade == input$decade_2) %>%
      arrange(desc(n)) %>%
      slice(1:20)
    
    plot_ly(data = nation_pairs, 
            x = ~n, 
            y = ~reorder(nation_pair, n),
            type = 'bar',
            text = n, 
            orientation = 'h',
            marker= list(color = 'rgb(158,202,225)',
                         line = list(color = 'rgb(8,48,107)',
                                     width = 1.5))) %>% 
      layout(xaxis = list(title ="Frequency"),
             yaxis = list(title = "")) %>%
      #tickangle = 330)) %>%
      config(displayModeBar = F) })
  
  
  speaker_favorite_words_count <- read_csv("~/projects/hansard-shiny/data/speakers/clean_speaker_favorite_words_by_decade.csv")
  
  layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
    # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
    gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
    gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
    gg
  }
  
  top_speaker_title <- function(input, output) {
      output$top_speaker_title <- renderText({
        
        paste("You have chosen a range that goes from",
                                        input$range[1], "to", input$range[2])})
    }
  
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
      print('NEED THIS')
      print(s)
      print(NN)
      #return(datatable(NN[NN$words_per_day==s$y,])) 
      NN <- NN[NN$words_per_day==s$y,]
      
      print(NN)
      
      aa <- NN$speaker
      
      print(aa)
      
      # read in this df first 
      speaker_favorite_words_count <- speaker_favorite_words_count %>%
        filter(speaker == aa)
      
      print(speaker_favorite_words_count)

      g <- ggplot(data = speaker_favorite_words_count) +
        geom_col(aes(x = reorder_within(word, n, decade), 
                     y = n),
                 fill = "skyblue3",
                 color = "black",
                 size = .3) +
                 #fill = "steel blue") +
        labs(title = paste0(aa, "'s top words over time"),
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
      print(s)
      print(NN)
      return(datatable(NN[NN$count_per_speech==s$y,])) 
    }) } 
  
  
  
  output$longest_debates <- renderPlotly({ 
    longest_debates <- read_csv("~/projects/hansard-shiny/data/debates/longest_debates.csv")
    
    render_value_3(longest_debates)
    
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
      print(s)
      print(NN)
      return(datatable(NN[NN$words_per_debate==s$y,])) 
    }) } 
  
  
  
  
  
  
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
                      choices = c("This", 
                                  "test")) } 
    else if (input$special_vocabulary == "cities") {
      
      
    }
  })
  
  
  tf_idf_a <- function(df, dct, dcb) {
    
    df <- df %>%
      filter(decade == dct | decade == dcb) 
    
    df <- df %>%
      bind_tf_idf(grammatical_collocates, decade, n)
    
    df <- df %>%
      #filter(decade == dct) %>%
      select(-n) %>%
      rename(n = tf_idf) 
    
    return(df)}
  
  

  

  
  
  
  
  output$collocates_top <- renderPlotly({
    
    collocates <- read_csv("~/projects/hansard-shiny/data/collocates/property_collocates.csv")
    
    collocates <- filter_sentiment(collocates, input$sentiment)
    
    
    collocates <- filter_noun(collocates, input$noun)
    
     if (input$collocate_measure == "count") {
       
     } else if (input$collocate_measure == "tf-idf") {
      
       collocates <- tf_idf_a(collocates, input$decade_collocates_top, input$decade_collocates_bottom)
       
       print(collocates)
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
      layout(xaxis = list(title ="Frequency"),
             yaxis = list(title = "")) %>%
      #tickangle = 330)) %>%
      config(displayModeBar = F) })
  
  
  output$collocates_bottom <- renderPlotly({
    collocates <- read_csv("~/projects/hansard-shiny/data/collocates/property_collocates.csv")
    
    collocates <- filter_sentiment(collocates, input$sentiment)
    
    if (input$collocate_measure == "count") {
      
    } else if (input$collocate_measure == "tf-idf") {
      
      collocates <- tf_idf_a(collocates, input$decade_collocates_top, input$decade_collocates_bottom)
      
      print(collocates)
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
      layout(xaxis = list(title ="Frequency"),
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
  
  
  
}

shinyApp(ui, server)



#layout(#autosize = F,
#   xaxis = xlab,
#   yaxis = ylab,
#  margin = list(l = 160), xaxis = list(tickangle = 45)) %>%

#config(displayModeBar = F)