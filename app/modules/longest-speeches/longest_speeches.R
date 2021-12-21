longest_speeches_ui <- function(id) {
  tagList(
    mainPanel(plotlyOutput(NS(id, "longest_speeches")),
              dataTableOutput(NS(id, 'tbl5')))
    )}


longest_speeches_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    l <- fread(paste0(data_dir, "speakers/longest_speeches.csv"))
    
    output$longest_speeches <- renderPlotly({ 
      
      render_value_2(l)
      
      plot_ly(data = l, 
              x = ~speechdate, 
              y = ~count_per_speech,
              source = "subset_2",
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
                             'Speech Word Count ', "<b>", count_per_speech, "</b>"),
              hoverinfo = 'text') %>%
        layout(xaxis = list(title = "Year"),
               yaxis = list(title = "Speech Word Count")) %>%
        config(displayModeBar = F) })

    render_value_2 = function(NN){
      output$tbl5 <- renderDataTable({
        s <- event_data("plotly_click", source = "subset_2")
        validate(need(!is.null(s), "Click on a point to see PLACEHOLDER"))
        
        return(datatable(NN[NN$count_per_speech==s$y,],
                         options = list(dom = 'ip'),
                         filter = list(position = "top")))}) }

  } ) }

