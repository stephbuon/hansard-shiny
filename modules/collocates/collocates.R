source("~/projects/hansard-shiny/modules/collocates/collocates_functions.R")

# js <- '
# $(document).on("keyup", function(e) {
#   if(e.keyCode == 13){
#     Shiny.onInputChange("keyPressed", Math.random());
#   }
# });
# '

collocates_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    #tags$script(js),
    
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_collocates"), 
                     "About This Page",
                     style = "color: #fff; 
                     background-color: #337ab7; 
                     border-color: #2e6da4; 
                     width: 179px;
                     padding:4px; 
                     font-size:90%"),
        p(),
        selectInput(NS(id, "vocabulary"), 
                    "Vocabulary:",
                    c("All" = "all",
                      "Property" = "property",
                      "Concerns" = "concerns",
                      "Nations" = "nations",
                      "Cities" = "cities")),
        
        selectInput(NS(id, "measure"),
                    "Measure:",
                    c("Count" = "count",
                      "tf-idf" = "tf-idf")),
        
        sliderTextInput(
          inputId = NS(id, "decade_collocates_top"), 
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
          inputId = NS(id,"decade_collocates_bottom"), 
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
        
        uiOutput(NS(id, "suggestion_1"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "suggestion_2"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "suggestion_3"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        br(),
        uiOutput(NS(id, "suggestion_4"),
                 onclick = paste0("Shiny.setInputValue('", ns('btnLabel'),"', this.innerText);")),
        
        textInput(NS(id, "custom_search"), 
                  "Custom Search:", ""),
        
        radioButtons(NS(id, "match_type"), 
                     "",
                     c("Includes Keyword" = "include",
                       "Matches Keyword" = "match"),
                     selected = "include"),
        
        sliderTextInput(
          inputId = NS(id,"sentiment"), 
          label = "Sentiment: ", 
          grid = TRUE, 
          force_edges = TRUE,
          choices = c("All",
                      "Positive", 
                      "Negative")),
        
        width = 2),
      
      mainPanel(plotlyOutput(NS(id, "collocates_top")),
                plotlyOutput(NS(id, "collocates_bottom"))) 
      ))}


collocates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$suggestion_1 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_1", label = "law", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_1", label = "tenant", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_1", label = "poor", style = "width: 179px;") }  })
    
    output$suggestion_2 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_2", label = "woman", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_2", label = "property", style = "width: 179px;") } 
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_2", label = "coal", style = "width: 179px;") }  })
    
    output$suggestion_3 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_3", label = "men", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_3", label = "rent", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_3", label = "future", style = "width: 179px;") } })
    
    output$suggestion_4 <- renderUI({
      if (input$vocabulary == "all") {
        actionButton("suggestion_4", label = "government", style = "width: 179px;") }
      else if (input$vocabulary == "property") {
        actionButton("suggestion_4", label = "landlord", style = "width: 179px;") }
      else if (input$vocabulary == "concerns") {
        actionButton("suggestion_4", label = "industry", style = "width: 179px;") }})
    
    
    observeEvent(input$btnLabel,{
      vals$btn=TRUE
      vals$text=FALSE })
    
    observeEvent(input$custom_search,{
      vals$btn=FALSE
      vals$text=TRUE })
    
    # observeEvent(input[["keyPressed"]], {
    #   vals$btn=FALSE
    #   vals$text=TRUE
    # })
    
    
    get_tf_idf <- reactive({
      if (input$measure == "tf-idf") {
        fname <- paste0("~/projects/hansard-shiny/app-data/collocates/tf-idf-data/tf_idf_", input$decade_collocates_top, "_", input$decade_collocates_bottom, "_", input$vocabulary, "_adj_noun_pairs.csv")
        fname_reverse <- paste0("~/projects/hansard-shiny/app-data/collocates/tf-idf-data/tf_idf_", input$decade_collocates_bottom, "_", input$decade_collocates_top, "_", input$vocabulary, "_adj_noun_pairs.csv")
        if (file.exists(fname)|file.exists(fname_reverse)) {
          if (file.exists(fname)) {
            df <- fread(fname) }
          else {
            df <- fread(fname_reverse) } }
        else {
          tf_idf_ct(collocates_top(), collocates_bottom(), input$vocabulary, input$custom_search, fname) } } }) 
    
    

    collocates_top <- reactive ({
      collocates_top <- import_collocates_data(input$decade_collocates_top, input$vocabulary)})
    
    collocates_bottom <- reactive ({
      collocates_bottom <- import_collocates_data(input$decade_collocates_bottom, input$vocabulary) })
    
    

    
    return_data <- function(collocates, measure, decade_slider, vals, match_type, custom_search, btnLabel, sentiment) {
      
      #if (measure == "count") {
        #collocates <- collocates_top() }
      if (measure == "tf-idf") {
        collocates <- get_tf_idf() }
      
      setkey(collocates, decade)
      collocates <- collocates[.(as.numeric(decade_slider))] 
      
      collocates <- search_ct(collocates, vals, match_type, custom_search, btnLabel)
      
      collocates <- filter_sentiment_ct(collocates, sentiment)
      
      collocates <- collocates[order(collocates, -n)]
      top <- collocates[1:20]
      
    }
    
    top_plot <- reactive ({ 
      return_data(collocates_top(), input$measure, input$decade_collocates_top, vals$text, input$match_type, input$custom_search, input$btnLabel, input$sentiment) }) #%>%
      #bindCache(collocates_top(), collocates_bottom(), input$measure, input$decade_collocates_top, vals$text, input$match_type, input$custom_search, input$btnLabel, input$sentiment)
    
    # bottom_plot <- reactive ({
    #   return_data(collocates, input$measure, input$decade_collocates_bottom, vals$text, input$match_type, input$custom_search, input$btnLabel, input$sentiment) }) %>%
    # 
    #   
    # })
    
    output$collocates_top <- renderPlotly({
      
       if (input$measure == "count") {
         xlab <- list(title ="Raw Count") }
       else if (input$measure == "tf-idf") {
         xlab <- list(title ="tf-idf") }

       plot_ly(data = top_plot(),
               x = ~n,
               y = ~reorder(grammatical_collocates, n),
               type = 'bar',
               text = n,
               orientation = 'h',
               marker= list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
         layout(title = input$decade_collocates_top,
                xaxis = xlab,
                yaxis = list(title = "")) %>%
         config(displayModeBar = F) }) #%>%
      #bindCache(collocates_top(), collocates_bottom(), input$measure, input$decade_collocates_top, vals$text, input$match_type, input$custom_search, input$btnLabel, input$sentiment)
    
    
    output$collocates_bottom <- renderPlotly({
       
      if (input$measure == "count") {
        collocates <- collocates_bottom()
        xlab <- list(title ="Raw Count") }
      
      else if (input$measure == "tf-idf") {
        collocates <- get_tf_idf()
        xlab <- list(title ="tf-idf") }
      
      setkey(collocates, decade)
      collocates <- collocates[.(as.numeric(input$decade_collocates_bottom))] 
      
      collocates <- search_ct(collocates, vals$text, input$match_type, input$custom_search, input$btnLabel)
      
      collocates <- filter_sentiment_ct(collocates, input$sentiment)
      
      collocates <- collocates[order(collocates, -n)]
      top <- collocates[1:20] 

      plot_ly(data = top, 
              x = ~n, 
              y = ~reorder(grammatical_collocates, n),
              type = 'bar',
              text = n, 
              orientation = 'h',
              marker= list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>% 
      layout(title = input$decade_collocates_bottom,
             xaxis = xlab,
             yaxis = list(title = "")) %>%
        config(displayModeBar = F) })
      
    
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
        "\"tf-idf\" is a numerical statistic that reflects how \"distinctive\" a word is to a corpus.
        The tf–idf value increases proportionally to the number of times a word appears in a decade and is offset by the other decade that contains the word,
        which helps to adjust the results for the fact that some words appear more frequently in general.",
        br(),
        p(),
        "\"jsd\" is a " )) })

    
  } ) }




# 
# ui <- fluidPage(
#   collocates_ui("collocates")
# )
# server <- function(input, output, session) {
#   collocates_server("collocates")
# }
# shinyApp(ui, server)  
#    

    