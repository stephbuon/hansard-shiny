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
        selectInput(NS(id, "sc_compare"), 
                    "Compare:",
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
                     style = "width: 179px;"
        ),
        
        width = 2),
      mainPanel(plotlyOutput(NS(id, "speaker_comparison_top")),
                plotlyOutput(NS(id, "speaker_comparison_bottom"))))
    
    
    
  ) }


speaker_comparison_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    h <- fread("~/projects/hansard-shiny/app-data/speakers/speaker_comparison_speaker_count_for_app_2.csv")
    j <- fread("~/projects/hansard-shiny/app-data/speakers/clean_clean_tokenized_hansard_counts.csv", key = "decade")
    
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
    
    
    
    
  } ) }

