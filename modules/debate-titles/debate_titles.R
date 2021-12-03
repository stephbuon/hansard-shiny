source("~/projects/hansard-shiny/modules/debate-titles/debate_titles_functions.R")

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
    
    output$debate_titles <- renderPlotly({
      
      debate_titles_data <- import_debate_titles_data()
      
      debate_titles_data <- keyword_addition(debate_titles_data, 
                                             words_per_year, 
                                             debate_title_year_counts, 
                                             input$kw_list, 
                                             input$keyword_addition, 
                                             input$keyword_addition_word_2, 
                                             input$keyword_addition_word_3, 
                                             input$keyword_addition_word_4, 
                                             input$keyword_addition_word_5, 
                                             input$keyword_addition_word_6)
      
      typed_search_terms <- stri_remove_empty(c(input$keyword_addition, 
                                  input$keyword_addition_word_2, 
                                  input$keyword_addition_word_3, 
                                  input$keyword_addition_word_4, 
                                  input$keyword_addition_word_5, 
                                  input$keyword_addition_word_6), TRUE)
      
      
      validate(need(length(debate_titles_data) > 0, "Type words in the text boxes to visualize their proportion over time"))
      
      render_value_debate_titles(input$kw_list, debate_titles_data, typed_search_terms)
      
      debate_titles_ggplot <- ggplot(data = debate_titles_data) +
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
        config(displayModeBar = F) })
    
    
    m <- fread("~/projects/hansard-shiny/app-data/debates/all_debate_titles_metadata.csv") %>%
      select(debate, year, year_count)
    
    render_value_debate_titles = function(input_kw_list, debate_titles_data, ...){
      output$debate_titles_DT <- renderDT({
        s <- event_data("plotly_click", source = "TEST")
        
        validate(need(!is.null(s), "Click on a bar to view the debates containing a keyword for that year"))
        
        if (input$kw_list != "custom") {
          metadata <- fread(paste0("~/projects/hansard-shiny/app-data/debates/kw_metadata_", input_kw_list, ".csv")) 
          test_2 <- left_join(metadata, debate_titles_data, by = c("keywords", "year")) }
        else {
          
          out <- data.table()
          for(i in 1:length(...)) {
            keyword <- ...[i]
            
            validate(need(!is.na(keyword), "Click on a bar to view the debates containing a keyword for that year"))
            
            debate_titles_w_keyword <- m %>%
              filter(str_detect(debate, regex(paste0("\\b", keyword, "\\b|^", keyword, "\\b|\\b", keyword, "$"), ignore_case = TRUE))) %>%
              mutate(keywords = keyword)
            
            out <- bind_rows(debate_titles_w_keyword, out) }
          
          debate_titles_data <- debate_titles_data %>%
            select(-year_count)
          
          test_2 <- left_join(out, debate_titles_data, by = c("keywords", "year", "debate")) } 
        
        test_2 <- test_2 %>%
          select(-debates_per_year, -words_per_year, -proportion)
        
        datatable(test_2[test_2$year==s$x,],
                  options = list(dom = 'ip'),
                  filter = list(position = "top")) }) }
    
    
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