top_speakers_ui <- function(id) {
  tagList(
    tabPanel("Top Speakers",
             sidebarLayout(
               sidebarPanel(
                 actionButton(NS(id, "about_top_speakers_by_year"), 
                              "About This Page",
                              style="color: #fff; 
                              background-color: #337ab7; 
                              border-color: #2e6da4; 
                              width: 179px;
                              padding:4px; 
                              font-size:90%"),
                 p(),
                 radioButtons(NS(id, "top_speakers_df"), 
                              "Top Speakers:",
                              c("By Year" = "by_year",
                                "By Decade" = "by_decade")),
                 width =2),
               mainPanel(plotlyOutput(NS(id, "top_speakers")),
                         br(),
                         br(),
                         plotlyOutput(NS(id, "tbl4"),
                                      height = "580"))) 
             )) } 


#layout_ggplotly <- function(gg, x = -0.02, y = -0.08){
# The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
#gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
#gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
#gg
#}

top_speakers_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    speaker_favorite_words_count <- fread(paste0(data_dir, "speakers/clean_speaker_favorite_words_by_decade.csv"))
    
    output$top_speakers <- renderPlotly({ 
      top_speakers <- fread(paste0(data_dir, "speakers/top_speakers_", input$top_speakers_df, ".csv"))
      
      render_value(top_speakers)
      
      plot_ly(data = top_speakers, 
              x = ~speechdate, 
              y = ~words_per_day,
              source = "subset",
              type = "scatter",
              marker = list(
                color = 'LightSkyBlue',
                size = 7,
                opacity = 0.8,
                line = list(
                  color = 'black',
                  width = 1 )),
              text = ~paste0('Name: ', "<b>", speaker, "</b>", '\n',
                             'Speech Date: ', "<b>", speechdate, "</b>", '\n',
                             'Word Count ', "<b>", words_per_day, "</b>"),
              hoverinfo = 'text') %>%
        layout(xaxis = list(title = "Year"),
               yaxis = list(title = "Word Count")) %>%
        config(displayModeBar = F) })


render_value = function(NN){
  output$tbl4 <- renderPlotly({
    s <- event_data("plotly_click", source = "subset")
    
    validate(need(!is.null(s), "Click on a point to view the speaker's top words over time"))
    
    NN <- NN[NN$words_per_day==s$y,]

    speaker_favorite_words_count <- speaker_favorite_words_count %>%
      filter(speaker == NN$speaker)
    
    g <- ggplot(data = speaker_favorite_words_count) +
      geom_col(aes(x = reorder_within(word, n, decade), 
                   y = n),
               fill = "skyblue3",
               color = "black",
               size = .3) +
      labs(title = paste0(NN$speaker, "'s Top Words Over Time"),
           subtitle = "",
           x = "Word",
           y = "Count") +
      scale_x_reordered() +
      facet_wrap(~ decade, scales = "free") + 
      coord_flip() + 
      theme_bw() + 
      theme(panel.spacing.y = unit(1, "lines"))
    
    #  make plots same size : https://stackoverflow.com/questions/45696723/how-can-i-make-plotly-subplots-the-same-size-when-converting-from-facets-with-gg
    ggplotly(g) %>%
      #layout_ggplotly %>%
      config(displayModeBar = F) }
  ) }  


observeEvent(input$about_top_speakers_by_year, {
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
    "Here we are using proportion instead of ENTER"))
})


} ) }