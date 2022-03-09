# compare units of speech for any two speakers for a given decade

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
        selectInput(NS(id, "measurement"), 
                    "Measure:",
                    c("Top Words" = "sc_top_words",
                      "tf-idf" = "tf-idf",
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
    
    # import radio button names
    radio_button_names <- fread(paste0(data_dir, "speakers/speaker_comparison_radio_button_names.csv"))
    
    
    # update radio button names if decade changes
    observe({
      radio_button_names <- radio_button_names %>% 
        filter(decade == input$sc_decade) 
      
      updateRadioButtons(session, "sc_radio_buttons_top",
                         choices = radio_button_names$clean_new_speaker,
                         selected = radio_button_names[1,3])
      
      updateRadioButtons(session, "sc_radio_buttons_bottom",
                         choices = radio_button_names$clean_new_speaker,
                         selected = radio_button_names[2,3] ) })
    
    
    # import data for visualization and set the decade column as index
    data_for_visualization <- reactive({
      if (input$unit == "tokens") {
        df <- fread(paste0(data_dir, "speakers/clean_clean_tokenized_hansard_counts.csv"), key = "decade") }
      else if (input$unit == "adj_noun_collocates") {
        df <- fread(paste0(data_dir, "speakers/clean_speaker_adj_noun_collocates.csv"), key = "decade") } })
    
    
    # switch between using the value of a radio button or the value of the text box 
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
    
    
    # return count or tf-idf values for top and bottom speakers
    apply_measurement <- function(data_for_visualization) { 
      
      if(top_vals_speaker_comparison$btn & bottom_vals_speaker_comparison$btn) {
        
        data_for_visualization <- calculate_results(data_for_visualization, 
                                                    input$sc_decade, 
                                                    input$measurement, 
                                                    input$sc_radio_buttons_top, 
                                                    input$sc_radio_buttons_bottom) }

      else if(top_vals_speaker_comparison$text & bottom_vals_speaker_comparison$btn) {
        
        data_for_visualization <- calculate_results(data_for_visualization, 
                                                    input$sc_decade, 
                                                    input$measurement, 
                                                    input$sc_radio_buttons_top, 
                                                    input$sc_radio_buttons_bottom) }

      else if (top_vals_speaker_comparison$btn & bottom_vals_speaker_comparison$text) {
        
        data_for_visualization <- calculate_results(data_for_visualization, 
                                                    input$sc_decade, 
                                                    input$measurement, 
                                                    input$sc_radio_buttons_top, 
                                                    input$sc_radio_buttons_bottom) }

      else if (top_vals_speaker_comparison$text & bottom_vals_speaker_comparison$text) {
        
        data_for_visualization <- calculate_results(data_for_visualization, 
                                                    input$sc_decade, 
                                                    input$measurement, 
                                                    input$sc_radio_buttons_top, 
                                                    input$sc_radio_buttons_bottom) }
      
      return(data_for_visualization) }
    
      
      # return results based for either top plot of bottom plot 
      which_plot <- function(data_for_visualization, is_top_or_bottom_plot) {
        
        # split the data so the top speaker is viualized on the top plot, and the bottom speaker is visualized on the bottom plot
        if(is_top_or_bottom_plot == "top") {
          
          if(top_vals_speaker_comparison$btn) {
            speaker <- input$sc_radio_buttons_top }
          else if (top_vals_speaker_comparison$text) {
            speaker <- input$sc_custom_search_top  }
          
          data_for_visualization <- data_for_visualization %>%
            filter(clean_new_speaker == speaker) %>%
            distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }
        
        
        else if(is_top_or_bottom_plot == "bottom") {
          
          if (bottom_vals_speaker_comparison$btn) {
            speaker <- input$sc_radio_buttons_bottom }
          else if (bottom_vals_speaker_comparison$text) {
            speaker <- input$sc_custom_search_bottom }
          
          data_for_visualization <- data_for_visualization %>%
            filter(clean_new_speaker == speaker) %>%
            distinct(new_speaker, decade, ngrams, n, clean_new_speaker) }
        
        filtered_data <- data_for_visualization[order(data_for_visualization, -n)][1:20]
        
        return(filtered_data) }

                
      
    
    
    output$speaker_comparison_top <- renderPlotly({
      
      data_for_visualization <- apply_measurement(data_for_visualization())
      
      top_plot <- which_plot(data_for_visualization, "top")
      
      if(top_vals_speaker_comparison$btn){
        title <- input$sc_radio_buttons_top } 
      else {
        title <- input$sc_custom_search_top }
      
      if (input$measurement == "sc_top_words") {
        xlab <- list(title ="Frequency") } 
      else if (input$measurement == "tf-idf") {
        xlab <- list(title ="tf-idf") }
      
      
      
      plot_ly(top_plot, 
              x = ~n, 
              y = ~reorder(ngrams, n), 
              type = 'bar', 
              hovertext = n,
              orientation = "h",
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
        layout(title = title,
               xaxis = list(title = xlab),
               yaxis = list(title = "")) %>%
        config(displayModeBar = F)})
    
    
    
    output$speaker_comparison_bottom <- renderPlotly({
      
      data_for_visualization <- apply_measurement(data_for_visualization()) 
      
      bottom_plot <- which_plot(data_for_visualization, "bottom")
      
      
      
      if(bottom_vals_speaker_comparison$btn){
        title <- input$sc_radio_buttons_bottom } 
      else {
        title <- input$sc_custom_search_bottom }
      
      if (input$measurement == "sc_top_words") {
        xlab <- list(title ="Frequency") } 
      else if (input$measurement == "tf-idf") {
        xlab <- list(title ="tf-idf") }
      


      plot_ly(bottom_plot, 
              x = ~n, 
              y = ~reorder(ngrams, n), 
              type = 'bar', 
              text = n,
              orientation = "h",
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
        layout(title = title,
               xaxis = list(title = xlab),
               yaxis = list(title = "")) %>%
        config(displayModeBar = F)})
    
    } ) }

