
top_vals_speaker_comparison = reactiveValues(btn = FALSE, text = FALSE)
bottom_vals_speaker_comparison = reactiveValues(btn = FALSE, text = FALSE)

speaker_comparison_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_nation_pairs"), 
                     "About This Page",
                     style="color: #fff;
                     background-color: #337ab7; 
                     border-color: #2e6da4; 
                     width: 179px;
                     padding:4px; 
                     font-size:90%"),
        p(),
        selectInput(NS(id, "unit"), 
                    "Unit:",
                    c("Tokens" = "tokens",
                      "Adjective-Noun Collocates" = "adj_noun_collocates")),
        selectInput(NS(id, "sc_compare"), 
                    "Measure:",
                    c("Top Words" = "sc_top_words",
                      "tf-idf" = "sc_tf-idf",
                      "Speech Lengths" = "sc_speech_lengths")),
        
        sliderTextInput(
          inputId = NS(id, "sc_decade"), 
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
        
        radioButtons(NS(id, "sc_radio_buttons_top"), 
                     "Top Speakers:",
                     #choices = NULL),
                     c("1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5"),
                     selected = "1"),
        
        textInput(NS(id, "sc_custom_search_top"), 
                  "Custom Search:", ""),
        
        radioButtons(NS(id, "sc_radio_buttons_bottom"), 
                     "Top Speakers:",
                     #choices = NULL),
                     c("1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5"),
                     selected = "1"),
        
        textInput(NS(id, "sc_custom_search_bottom"), 
                  "Custom Search:", ""),
        
        actionButton(NS(id, 'download_speaker_comparison'), 
                     "Download Plot",
                     style = "width: 179px;"),
        width = 2),
      
      mainPanel(plotlyOutput(NS(id, "speaker_comparison_top")),
                plotlyOutput(NS(id, "speaker_comparison_bottom"))))
    
    
    
  ) }


speaker_comparison_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    h <- fread(paste0(data_dir, "speakers/speaker_comparison_radio_button_names.csv"))
    
    j <- reactive({
      if (input$unit == "tokens") {
        a <- fread(paste0(data_dir, "speakers/clean_clean_tokenized_hansard_counts.csv"), key = "decade") }
      else if (input$unit == "adj_noun_collocates") {
        a <- fread(paste0(data_dir, "speakers/clean_speaker_adj_noun_collocates.csv"), key = "decade") } })
    
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
         filter(decade == input$sc_decade) 
      
      updateRadioButtons(session, "sc_radio_buttons_top",
                         choices = h$clean_new_speaker,
                         selected = h[1,3])
      
      updateRadioButtons(session, "sc_radio_buttons_bottom",
                         choices = h$clean_new_speaker,
                         selected = h[2,3] ) })
    
   # xlab<- reactive({
   #    if (input$sc_compare == "sc_top_words") {
   #      xlab <- list(title ="Frequency") } 
   #    else if (input$sc_compare == "sc_tf-idf") {
   #      xlab <- list(title ="tf-idf") }
   #    
   #  })
    
    
    test_2 <- function (j, top_or_bottom) { # search for speakers 
      
      if (top_vals_speaker_comparison$btn & bottom_vals_speaker_comparison$btn) {
        j <- binary_search(j, input$sc_decade, input$sc_compare, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) }

      else if (top_vals_speaker_comparison$text & bottom_vals_speaker_comparison$btn) {
        j <- binary_search(j, input$sc_decade, input$sc_compare, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) }

      else if (top_vals_speaker_comparison$btn & bottom_vals_speaker_comparison$text) {
        j <- binary_search(j, input$sc_decade, input$sc_compare, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) }

      else if (top_vals_speaker_comparison$text & bottom_vals_speaker_comparison$text) {
        j <- binary_search(j, input$sc_decade, input$sc_compare, input$sc_radio_buttons_top, input$sc_radio_buttons_bottom) }
      

      if (top_or_bottom == "top") {

        if (top_vals_speaker_comparison$btn) {
          speaker <- input$sc_radio_buttons_top }
        else if (top_vals_speaker_comparison$text) {
          speaker <- input$sc_custom_search_top  }

          j <- j %>%
            filter(clean_new_speaker == speaker) %>%
            distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }


      else if (top_or_bottom == "bottom") {

        if (bottom_vals_speaker_comparison$btn) {
          speaker <- input$sc_radio_buttons_bottom }
        else if (bottom_vals_speaker_comparison$text) {
          speaker <- input$sc_custom_search_bottom }

          j <- j %>%
            filter(clean_new_speaker == speaker) %>%
            distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }
      
      top <- j[order(j, -n)][1:20]

      return(top) }
    
    
    output$speaker_comparison_top <- renderPlotly({
      
      ll <- test_2(j(), "top") 
      
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
              hovertext = n,
              orientation = "h",
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
        layout(title = ff,
               xaxis = list(title = xlab),
               yaxis = list(title = "")) %>%
        config(displayModeBar = F)})
    
    
    
    
    
    # IF TEXT IS TRUE GET RID OF RADIO BOX 
    
    
    
    output$speaker_comparison_bottom <- renderPlotly({
      
      ll <- test_2(j(), "bottom") 
      
      
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
        config(displayModeBar = F)})
    
    } ) }

