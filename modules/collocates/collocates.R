# not written for efficency

collocates_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_collocates"), 
                     "About This Page",
                     style="color: #fff; 
                                       background-color: #337ab7; 
                                       border-color: #2e6da4; 
                                       width: 179px;
                                       padding:4px; 
                                       font-size:90%"),
        p(),
        #helpText("Choose a vocabulary list."),
        radioButtons(NS(id, "special_vocabulary"), 
                     "Special Vocabulary:",
                     c("Property" = "property",
                       "Concerns" = "concerns",
                       "Offices" = "offices",
                       "Nations" = "nations",
                       "Cities" = "cities")),
        
        #tags$hr(style="border-color: black;"),
        #helpText("Slide the dial to change decades."),
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
        
        selectInput(NS(id,"noun"), 
                    "Keyword:",
                    choices = NULL),
        
        
        #tags$hr(style="border-color: black;"),
        
        sliderTextInput(
          inputId = NS(id,"sentiment"), 
          label = "Sentiment: ", 
          grid = TRUE, 
          force_edges = TRUE,
          choices = c("All",
                      "Positive", 
                      "Negative")),
        
        radioButtons(NS(id,"collocate_measure"),
                     "Measure:", 
                     list("count", "tf-idf", "jsd"), inline = TRUE, selected = "count"),
        
        
        width = 2),
      
      mainPanel(plotlyOutput(NS(id,"collocates_top")),
                plotlyOutput(NS(id,"collocates_bottom"))))
    
    
    
  ) }



collocates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

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
  collocates_data <- fread(paste0("~/projects/hansard-shiny/app-data/collocates/", input$special_vocabulary, "_collocates.csv")) })

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
    "\"jsd\" is a " ))
})


})}





