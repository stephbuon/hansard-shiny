collocates_search = reactiveValues(btn = FALSE, text = FALSE)

collocates_2_ui <- function(id) {
  tagList(
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
        
        actionButton(NS(id, "placeholder"), "suggestion"),
        actionButton(NS(id, "placeholder_2"), "suggestion"),
        actionButton(NS(id, "placeholder_3"), "suggestion"),
        actionButton(NS(id, "placeholder_4"), "suggestion"),
        
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
                plotlyOutput(NS(id, "collocates_bottom"))
        
      ))
    
    
    
    
  ) }



collocates_2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
# 
#     observeEvent(input$btnLabel,{
#       vals$btn=TRUE
#       vals$text=FALSE })
    
    observeEvent(input$custom_search,{
      #vals$btn=FALSE
      collocates_search$text=TRUE })
    
    
    

    filter_sentiment <- function(df, sw) {
      if(sw == "All") {
        return(df) }
      df <- df %>%
        filter(sentiment == sw)
      return (df) }


    # filter_noun <- function(df, nnn) {
    #   if(nnn == 'All') {
    #     return(df) } else {
    #       df <- df %>%
    #         filter(str_detect(grammatical_collocates, paste0(" ", nnn))) } }
    # 
    
    
    # observe({
    #   if (input$special_vocabulary == "property") {
    #     updateSelectInput(session = session, 
    #                       inputId = "noun", 
    #                       choices = c("Select" = "All", 
    #                                   "Evict" = "evict",
    #                                   "Inclosure" = "inclosure",
    #                                   "Land" = "land$",
    #                                   "Landhold" = "landhold",
    #                                   "Landlord" = "landlord",
    #                                   "Lease" = "lease",
    #                                   "Lessee" = "lessee",
    #                                   "Rent" = "rent",
    #                                   "Tenant" = "tenan")) } 
    #   
    #   else if (input$special_vocabulary == "concerns") {
    #     updateSelectInput(session = session, 
    #                       inputId = "noun", 
    #                       choices = c("Select" = "All", 
    #                                   "Poor" = "poor")) } 
    #   else if (input$special_vocabulary == "cities") {
    #     
    #     
    #   }
    # })
    
   # maybe I can do !file.exists(fname)|!file.exists(fname_reverse) thing later
    # tf_idf_b <- reactive({
    #   if (input$measure == "tf-idf") {
    #     fname <- paste0("~/projects/hansard-shiny/app-data/collocates_2/tf-idf-data/tf_idf_", input$decade_collocates_top, "_", input$decade_collocates_bottom, "_", input$vocabulary, "_adj_noun_pairs.csv")
    #     fname_reverse <- paste0("~/projects/hansard-shiny/app-data/collocates_2/tf-idf-data/tf_idf_", input$decade_collocates_bottom, "_", input$decade_collocates_top, "_", input$vocabulary, "_adj_noun_pairs.csv")
    #     if(!file.exists(fname)) {
    #       tf_idf_a(collocates_top(), collocates_bottom(), fname) }
    #     else {
    #       df <- fread(fname) 
    #       }
    #   
    #   
    # } }) 
    
    tf_idf_b <- reactive({
      if (input$measure == "tf-idf") {
        fname <- paste0("~/projects/hansard-shiny/app-data/collocates_2/tf-idf-data/tf_idf_", input$decade_collocates_top, "_", input$decade_collocates_bottom, "_", input$vocabulary, "_adj_noun_pairs.csv")
        fname_reverse <- paste0("~/projects/hansard-shiny/app-data/collocates_2/tf-idf-data/tf_idf_", input$decade_collocates_bottom, "_", input$decade_collocates_top, "_", input$vocabulary, "_adj_noun_pairs.csv")
        if (file.exists(fname)|file.exists(fname_reverse)) {
          if (file.exists(fname)) {
            df <- fread(fname) }
          else {
            df <- fread(fname_reverse)
          } } 
        
        else {
          tf_idf_a(collocates_top(), collocates_bottom(), fname) 
        }
        
        
      } }) 
    
    
    tf_idf_a <- function(df1, df2, fname) {
      
      if (input$custom_search!="") { 
        df1 <- df1 %>%
          filter(str_detect(grammatical_collocates, regex(input$custom_search, ignore_case = T)))
        df2 <- df2 %>%
          filter(str_detect(grammatical_collocates, regex(input$custom_search, ignore_case = T))) }
      
      df <- bind_rows(df1, df2)
     
      df <- df %>%
        bind_tf_idf(grammatical_collocates, decade, n)
      
      df <- df %>%
        select(-n) %>%
        rename(n = tf_idf) 
      
      fwrite(df, fname)
      
      return(df)}
    
    
    import_collocates_data <- function(slider) {
      collocates_data <- fread(paste0("~/projects/hansard-shiny/app-data/collocates_2/", "clean_", input$vocabulary, "_adj_noun_collocates.csv"), key = "decade")
      collocates_data <- collocates_data[.(as.numeric(slider))]
      return(collocates_data) }
    
    collocates_top <- reactive ({
      collocates_top <- import_collocates_data(input$decade_collocates_top)})
    
    collocates_bottom <- reactive ({
      collocates_bottom <- import_collocates_data(input$decade_collocates_bottom)
    })
    
    
    
    output$collocates_top <- renderPlotly({
      
       if (input$measure == "count") {
         collocates <- collocates_top()
         if (input$custom_search!="") { 
           collocates <- collocates %>%
             filter(str_detect(grammatical_collocates, regex(input$custom_search, ignore_case = T))) } else {
               
               
             }
         
         
         xlab <- list(title ="Raw Count") }
       else if (input$measure == "tf-idf") {
         collocates <- tf_idf_b()
         xlab <- list(title ="tf-idf") }
 
      setkey(collocates, decade)
      collocates <- collocates[.(as.numeric(input$decade_collocates_top))] 
      
      
      
      collocates <- filter_sentiment(collocates, input$sentiment)
      
      # collocates <- filter_noun(collocates, input$noun)

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
         layout(title = input$decade_collocates_top,
                xaxis = xlab,
                yaxis = list(title = "")) %>%
         config(displayModeBar = F) })
    
    
    output$collocates_bottom <- renderPlotly({
       
      if (input$measure == "count") {
        collocates <- collocates_bottom()
        
        if (input$custom_search!="") { 
          collocates <- collocates %>%
            filter(str_detect(grammatical_collocates, regex(input$custom_search, ignore_case = T))) } else {
              
              
            }
        xlab <- list(title ="Raw Count") }
      else if (input$measure == "tf-idf") {
        collocates <- tf_idf_b()
        xlab <- list(title ="tf-idf") }
      
      setkey(collocates, decade)
      collocates <- collocates[.(as.numeric(input$decade_collocates_bottom))] 
      
      collocates <- filter_sentiment(collocates, input$sentiment)
      
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
#   collocates_2_ui("collocates")
# )
# server <- function(input, output, session) {
#   collocates_2_server("collocates")
# }
# shinyApp(ui, server)  
#    

    