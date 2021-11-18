debate_titles_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(NS(id, "about_debate_titles"), 
                     "About This Page",
                     style="color: #fff; 
                     background-color: #337ab7; 
                     border-color: #2e6da4; 
                     width: 179px;
                     padding:4px; 
                     font-size:90%"),
        
        p(),
        
        selectInput(NS(id, "kw_list"), 
                    "Keyword List:",
                    list(`Special Vocabulary` = list("Property" = "property",
                                                     "Transportation" = "transportation",
                                                     "Resources" = "resources",
                                                     "Industry" = "industry"),
                         `Custom Vocabulary` = list("Blank Plot" = "custom"))),
        
        textInput(NS(id,"keyword_addition"), 
                  "Search Terms:", ""),
        textInput(NS(id,"keyword_addition_word_2"), 
                  "", ""),
        textInput(NS(id, "keyword_addition_word_3"), 
                  "", ""),
        textInput(NS(id, "keyword_addition_word_4"), 
                  "", ""),
        textInput(NS(id, "keyword_addition_word_5"), 
                  "", ""),
        textInput(NS(id, "keyword_addition_word_6"), 
                  "", ""),
        
        actionButton(NS(id, 'download'), 
                     "Download Plot",
                     style = "width: 179px;"),
        
        width = 2),
      
      mainPanel(plotlyOutput(NS(id, "debate_titles")),
      
      tags$script('document.getElementById("debate_titles-download").onclick = function() {
      var gd = document.getElementById("debate_titles-debate_titles");
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
      
      DTOutput(NS(id, 'debate_titles_DT'))))
    
    
     ) }



debate_titles_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    words_per_year <- fread("~/projects/hansard-shiny/app-data/debates/words_per_year.csv")
    debate_title_year_counts <- fread("~/projects/hansard-shiny/app-data/debates/debate_title_year_counts.csv")
    
    
    
    import_debate_titles_data <- reactive( {
      if (input$kw_list != "custom") {
        kw_data <- fread(paste0("~/projects/hansard-shiny/app-data/debates/kw_list_", input$kw_list, ".csv")) } 
      else { 
        kw_data <- data.table() } })
    
    
    
    keyword_addition <- function(d, input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6){
      
      keywords <- stri_remove_empty(c(input_keyword_addition, input_keyword_addition_word_2, input_keyword_addition_word_3, input_keyword_addition_word_4, input_keyword_addition_word_5, input_keyword_addition_word_6), TRUE)
      
        
        if (input$kw_list != "custom") { 
          all_year_counts <- fread(paste0("~/projects/hansard-shiny/app-data/debates/all_year_counts_", input$kw_list, ".csv")) } 
        else {
          all_year_counts <- fread("~/projects/hansard-shiny/app-data/debates/all_debate_titles_metadata.csv") }
      
      if (length(keywords != 0)) {
        
        
        for (keyword in keywords) {
          if (! keyword %in% all_year_counts$keywords) {
          
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
          
          all_year_counts <- bind_rows(all_year_counts, year_count) } }
        
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
    
    
    m <- fread("~/projects/hansard-shiny/app-data/debates/all_debate_titles_metadata.csv") %>%
      select(debate, year, year_count)
    
    render_value_debate_titles = function(NN, ...){
      output$debate_titles_DT <- renderDT({
        s <- event_data("plotly_click", source = "TEST")
        
        validate(need(!is.null(s), "Click on a bar to view the debates containing a keyword for that year"))
        
        if (input$kw_list != "custom") {
          metadata <- fread(paste0("~/projects/hansard-shiny/app-data/debates/kw_metadata_", input$kw_list, ".csv")) 
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
    
    
    
  }) }